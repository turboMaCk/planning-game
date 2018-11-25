{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}

module AgilePoker.Server (main, genContext) where

import Servant
import Data.Maybe (maybe)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Network.HTTP.Types (status200)
import Data.ByteString (ByteString)
import Data.Maybe (maybe)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.Wai (Request, Response, Middleware, requestHeaders, responseFile, rawPathInfo)
import Servant.API.WebSocket (WebSocket)
import Control.Exception (finally)
import Network.Wai.Middleware.Static (Policy, staticPolicy, addBase)
import Web.Cookie (parseCookies)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Network.WebSockets as WS
import qualified Control.Concurrent as Concurrent


-- Modules


import AgilePoker.Session
import AgilePoker.Event
import AgilePoker.UserInfo
import AgilePoker.Table


-- State


type ServerState = Sessions


-- Authorization


lookupSession :: MVar ServerState -> SessionId -> Handler Session
lookupSession state' sessionId = do
  state <- liftIO $ Concurrent.readMVar state'
  case getSession sessionId state of
    Just session ->
      pure session

    Nothing ->
      throwError $ err403 { errBody = "Invalid SessionId" }


-- | This function takes extract session id from header value
-- Correct format is `Bearer xxxx` where xxxx is a SessionID itself
parseSessionId :: ByteString -> Maybe SessionId
parseSessionId headerVal = BS.stripPrefix "Bearer " headerVal


authHeaderHandler :: MVar ServerState -> AuthHandler Request Session
authHeaderHandler state = mkAuthHandler handler
  where
    maybeToEither e =
      maybe (Left e) Right

    throw401 msg =
        throwError $ err401 { errBody = msg }

    mSessionId :: Request -> Maybe SessionId
    mSessionId req =
        parseSessionId
            =<< lookup "Authorization" (requestHeaders req)

    handler :: Request -> Handler Session
    handler req = either throw401 (lookupSession state) $ do
        maybeToEither "Missing SessionId" $ mSessionId req


-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "header") = Session


authCookieHandler :: MVar ServerState -> AuthHandler Request Session
authCookieHandler state = mkAuthHandler handler
  where
  maybeToEither e =
    maybe (Left e) Right

  throw401 msg =
    throwError $ err401 { errBody = msg }

  mSessionId :: Request -> Maybe SessionId
  mSessionId req =
    lookup "authorization" . parseCookies
        =<< lookup "cookie" (requestHeaders req)

  handler :: Request -> Handler Session
  handler req = either throw401 (lookupSession state) $ do
    maybeToEither "Missing SessionId" $ mSessionId req


-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie") = Session


-- API


type Api = "status"                                :> Get  '[JSON] T.Text
      :<|> "session" :> AuthProtect "header"       :> Get  '[JSON] Session
      :<|> "join"    :> ReqBody '[JSON] UserInfo   :> Post '[JSON] T.Text
      :<|> "stream"  :> AuthProtect "cookie"       :> WebSocket
      :<|> "table"   :> Capture "tableid" TableId  :> "join"
                     :> ReqBody '[JSON] UserInfo   :> Post '[JSON] T.Text
      :<|> "table"   :> ReqBody '[JSON] UserInfo   :> Post '[JSON] T.Text


api :: Proxy Api
api = Proxy


-- Server


genContext :: MVar ServerState -> Context (AuthHandler Request Session : AuthHandler Request Session ': '[])
genContext state = authCookieHandler state :. authHeaderHandler state :. EmptyContext


server :: MVar ServerState -> Server Api
server state' = status
           :<|> getSession
           :<|> join
           :<|> stream
           :<|> joinRoom
           :<|> createRoom

  where
    status :: Handler T.Text
    status = pure "OK"

    getSession :: Session -> Handler Session
    getSession = pure

    join :: UserInfo -> Handler T.Text
    join UserInfo { userName=name } = do
      mSession <- liftIO $ Concurrent.modifyMVar state' $ addSession name

      case mSession of
        Just ( id', session ) -> do
            state <- liftIO $ Concurrent.readMVar state'

            -- broadcast join event
            liftIO $ broadcast state $ userJoined session

            pure $ TE.decodeUtf8 id'

        Nothing ->
            throwError $ err409 { errBody = "Name already taken" }

    stream :: MonadIO m => Session -> WS.Connection -> m ()
    stream session = liftIO . handleSocket state' session

    -- @TODO: implement
    joinRoom :: TableId -> UserInfo -> Handler T.Text
    joinRoom id' UserInfo { userName=name } = pure "hi!"

    createRoom :: UserInfo -> Handler T.Text
    createRoom UserInfo { userName=name } =
      pure "hi"


broadcast :: ServerState -> Event -> IO ()
broadcast state' event = do
    forM_ state' $ \(Session { sessionConnections=conns }) ->
      forM_ conns $ flip WS.sendTextData $ encodeEvent event


handleSocketEvent :: MVar ServerState -> WS.Connection -> IO ()
handleSocketEvent state' conn = forever $ do
  msg :: BS.ByteString <- WS.receiveData conn
  -- state <- Concurrent.readMVar state'
  -- @TODO: implement
  pure ()


handleSocket :: MVar ServerState -> Session -> WS.Connection -> IO ()
handleSocket state' session conn = do
  let sessionId' = sessionId session

  -- assing connection
  mConnectionId <- Concurrent.modifyMVar state' $ pure . assignConnection sessionId' conn

  -- Sync state to new user
  state <- Concurrent.readMVar state'
  forM_ state $ WS.sendTextData conn . encodeEvent . userJoined

  case mConnectionId of
    Just ( connectionId, session ) ->
        -- Disconnect user at the end of session
        flip finally (disconnect ( sessionId', connectionId )) $ do
            -- ping thread
            WS.forkPingThread conn 30

            -- broadcast join event
            state <- Concurrent.readMVar state'
            broadcast state $ userStatusUpdate session

            -- assign handler
            handleSocketEvent state' conn

    Nothing ->
      -- @TODO: Add error handling
      -- but this is very unlikely situation
      pure ()

  where
    disconnect :: ( SessionId, Int ) -> IO ()
    disconnect id' = do
      -- disconnect
      Concurrent.modifyMVar_ state' $
        pure . disconnectSession id'

      -- broadcast
      state <- Concurrent.readMVar state'
      let session = getSession (fst id') state

      maybe (pure ()) (broadcast state . userStatusUpdate) session


indexMiddleware :: Middleware
indexMiddleware application request respond =
    if rawPathInfo request == "/"
    then respond indexRes
    else application request respond

  where
    indexRes :: Response
    indexRes = responseFile status200 headers fileName Nothing
        where
            fileName = "public/index.html"
            headers = [ ( "Content-Type", "text/html" )
                      , ( "Cache-Control", "public, max-age=86400" )
                      ]


app :: MVar ServerState -> Application
app state =
  staticPolicy static
    $ indexMiddleware
    $ serveWithContext api (genContext state) $ server state

  where
    static :: Policy
    static = addBase "public"


main :: IO ()
main = do
  state <- Concurrent.newMVar emptySessions
  Warp.run 3000 $ app state
