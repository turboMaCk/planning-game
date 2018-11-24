{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}

module AgilePocker.Server (main) where

import Servant
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Network.HTTP.Types (status200)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.Wai (Request, Response, Middleware, requestHeaders, responseFile, rawPathInfo)
import Servant.API.WebSocket (WebSocket)
import Control.Exception (finally)
import Network.Wai.Middleware.Static (Policy, staticPolicy, addBase)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Network.WebSockets as WS
import qualified Control.Concurrent as Concurrent


-- Modules


import AgilePocker.Session


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
      throwError (err403 { errBody = "Invalid SessionID" })


--- | This function takes extract session id from header value
--- Correct format is `Bearer xxxx` where xxxx is a SessionID itself
parseSessionId :: BS.ByteString -> Maybe SessionId
parseSessionId headerVal = BS.stripPrefix "Bearer " headerVal


--- | The auth handler wraps a function from Request -> Handler Account.
--- We look for a token in the request headers that we expect to be in the cookie.
--- The token is then passed to our `lookupAccount` function.
authHandler :: MVar ServerState -> AuthHandler Request Session
authHandler state = mkAuthHandler handler
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
        maybeToEither "Missing SessionID" $ mSessionId req


-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "session-id") = Session


-- API


type Api = "status"  :> Get '[JSON] T.Text
      :<|> "stream" :> WebSocket


api :: Proxy Api
api = Proxy


-- Server


-- | The context that will be made available to request handlers. We supply the
-- "session-id"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genContext :: MVar ServerState -> Context (AuthHandler Request Session ': '[])
genContext state = authHandler state :. EmptyContext


broadcast :: T.Text -> ServerState -> IO ()
broadcast message state' = do
    T.putStrLn message
    forM_ state' $ \(Session { sessionConnection=conn }) ->
      WS.sendTextData conn message


handleSocketEvent :: MVar ServerState -> WS.Connection -> IO ()
handleSocketEvent state' conn = forever $ do
  msg <- WS.receiveData conn
  state <- Concurrent.readMVar state'
  broadcast msg state


handleSocket :: MVar ServerState -> WS.Connection -> IO ()
handleSocket state' conn = do
  -- @TODO: hardcoded name
  sessionId <- Concurrent.modifyMVar state' $ addSession "Joe Doe" conn

  -- Disconnect user at the end of session
  flip finally (disconnect sessionId) $ do
    WS.forkPingThread conn 30
    handleSocketEvent state' conn

  where
    disconnect :: SessionId -> IO ()
    disconnect id' =
      Concurrent.modifyMVar_ state' $ pure . removeSession id'


server :: MVar ServerState -> Server Api
server state' = status
           :<|> joinRoom

  where
    status :: Handler T.Text
    status = pure "OK"

    joinRoom :: MonadIO m => WS.Connection -> m ()
    joinRoom = liftIO . handleSocket state'


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


static :: Policy
static = addBase "public"


main :: IO ()
main = do
  state <- Concurrent.newMVar emptySessions
  Warp.run 3000 $ app state
