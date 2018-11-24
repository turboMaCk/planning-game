{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}

module AgilePocker.Server (main, genContext) where

import Servant
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Network.HTTP.Types (status200)
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


import AgilePocker.Session
import AgilePocker.Event
import AgilePocker.UserInfo


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
      throwError $ err403 { errBody = "Invalid SessionID" }


-- | This function takes extract session id from header value
-- Correct format is `Bearer xxxx` where xxxx is a SessionID itself
parseSessionId :: BS.ByteString -> Maybe SessionId
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


type Api = "status" :> Get '[JSON] T.Text
      :<|> "join"   :> ReqBody '[JSON] UserInfo :> Post '[JSON] T.Text
      :<|> "stream" :> AuthProtect "cookie" :> WebSocket


api :: Proxy Api
api = Proxy


-- Server


genContext :: MVar ServerState -> Context (AuthHandler Request Session : AuthHandler Request Session ': '[])
genContext state = authCookieHandler state :. authHeaderHandler state :. EmptyContext


server :: MVar ServerState -> Server Api
server state' = status
           :<|> join
           :<|> stream

  where
    status :: Handler T.Text
    status = pure "OK"

    join :: UserInfo -> Handler T.Text
    join UserInfo { userName=name } = do
      id' <- liftIO $ Concurrent.modifyMVar state' $ addSession name
      pure $ TE.decodeUtf8 id'

    stream :: MonadIO m => Session -> WS.Connection -> m ()
    stream session = liftIO . handleSocket state' session


broadcast :: ServerState -> Event -> IO ()
broadcast state' event = do
    forM_ state' $ \(Session { sessionConnection=conn }) ->
       maybe (pure ()) (flip WS.sendTextData $ encodeEvent event) conn


handleSocketEvent :: MVar ServerState -> WS.Connection -> IO ()
handleSocketEvent state' conn = forever $ do
  msg :: BS.ByteString <- WS.receiveData conn
  -- state <- Concurrent.readMVar state'
  -- @TODO: implement
  pure ()


handleSocket :: MVar ServerState -> Session -> WS.Connection -> IO ()
handleSocket state' session conn = do
  let sessionId' = sessionId session
  state <- Concurrent.readMVar state'

  -- Sync state to new user
  forM_ state $ WS.sendTextData conn . encodeEvent . userJoined

  -- assing connection
  Concurrent.modifyMVar_ state' $ pure . assignConnection sessionId' conn

  -- Disconnect user at the end of session
  flip finally (disconnect sessionId' session) $ do

    -- ping thread
    WS.forkPingThread conn 30

    -- broadcast join event
    broadcast state $ userJoined session

    -- assign handler
    handleSocketEvent state' conn

  where
    disconnect :: SessionId -> Session -> IO ()
    disconnect id' session = do
      Concurrent.modifyMVar_ state' $ pure . removeSession id'
      state <- Concurrent.readMVar state'
      broadcast state $ userLeft session


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
