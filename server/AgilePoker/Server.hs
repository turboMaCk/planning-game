{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}

module AgilePoker.Server (run) where

import Servant
import Data.Maybe (maybe)
import Data.ByteString (ByteString)
import Data.Maybe (maybe)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.Wai (Response)
import Servant.API.WebSocket (WebSocket)
import Control.Exception (finally)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Network.WebSockets as WS
import qualified Control.Concurrent as Concurrent

import AgilePoker.Server.Static
import AgilePoker.Session
import AgilePoker.Event
import AgilePoker.Table
import AgilePoker.Server.Authorization
import AgilePoker.Api.UserInfo
import AgilePoker.Player


-- State


data ServerState = ServerState
  { sessions :: MVar Sessions
  , tables :: MVar Tables
  }


initialSessions :: IO ServerState
initialSessions = ServerState
  <$> Concurrent.newMVar emptySessions
  <*> Concurrent.newMVar emptyTables


-- API


type Api = "status"                                :> Get  '[JSON] T.Text
      :<|> "session"                               :> Post '[JSON] Session
      :<|> "session" :> AuthProtect "header"       :> Get  '[JSON] Session
      :<|> "tables"  :> AuthProtect "header"       :> ReqBody '[JSON] UserInfo   :> Post '[JSON] Table
      :<|> "tables"  :> AuthProtect "header"       :> Capture "tableid" TableId  :> "join"
                     :> ReqBody '[JSON] UserInfo   :> Post '[JSON] Table
      :<|> "tables"  :> AuthProtect "header"       :> Capture "tableid" TableId  :> "me"
                                                   :> Get '[JSON] Player
      -- :<|> "tables"  :> AuthProtect "cookie"       :> Capture "tableid" TableId  :> "stream" :> WebSocket


api :: Proxy Api
api = Proxy


-- Server


genContext :: MVar Sessions -> Context (SessionAuth ': '[])
genContext state =
  authHeaderHandler state :. EmptyContext


server :: ServerState -> Server Api
server state = status
           :<|> createSession
           :<|> getSession
           :<|> createTableHandler
           :<|> joinTableHandler
           :<|> meHandler
           -- :<|> streamTableHandler

  where
    status :: Handler T.Text
    status = pure "OK"

    createSession :: Handler Session
    createSession = pure . snd =<<
      (liftIO $ Concurrent.modifyMVar (sessions state) $ addSession "")

    getSession :: Session -> Handler Session
    getSession = pure

    meHandler :: Session -> TableId -> Handler Player
    meHandler session tableId = do
      ts <- liftIO $ Concurrent.readMVar (tables state)
      mPlayer <- liftIO $ getTablePlayer session tableId ts

      case mPlayer of
        Just player ->
          pure player

        Nothing ->
          throwError $ err401 { errBody = "You're not a player of this table" }


    -- join :: UserInfo -> Handler T.Text
    -- join UserInfo { userName=name } = do
    --   ( id', session ) <- liftIO $ Concurrent.modifyMVar (sessions state) $ addSession name

    --   s <- liftIO $ Concurrent.readMVar (sessions state)

    --   -- broadcast join event
    --   liftIO $ broadcast s $ userJoined session

    --   pure $ TE.decodeUtf8 id'

    createTableHandler :: Session -> UserInfo -> Handler Table
    createTableHandler session UserInfo { userName=name } =
      liftIO $ Concurrent.modifyMVar (tables state) $
        createTable session name

    joinTableHandler :: Session -> TableId -> UserInfo -> Handler Table
    joinTableHandler session id' UserInfo { userName=name } = do
      tables <- liftIO $ Concurrent.readMVar (tables state)
      mTable <- liftIO $ joinTable session id' name tables

      case mTable of
        Just table ->
          pure table

        Nothing ->
          throwError $ err409 { errBody = "Name already taken" }

    streamTableHandler :: MonadIO m => Session -> TableId -> WS.Connection -> m ()
    streamTableHandler session id' =
      liftIO . tableStreamHandler (tables state) session id'


broadcast :: Sessions -> Event -> IO ()
broadcast state' event = do
    forM_ state' $ \(Session { sessionConnections=conns }) ->
      forM_ conns $ flip WS.sendTextData $ encodeEvent event


handleSocketEvent :: MVar Sessions -> WS.Connection -> IO ()
handleSocketEvent state' conn = forever $ do
  msg :: ByteString <- WS.receiveData conn
  -- state <- Concurrent.readMVar state'
  -- @TODO: implement
  pure ()


handleSocket :: MVar Sessions -> Session -> WS.Connection -> IO ()
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



app :: ServerState -> Application
app state = staticMiddleware $
    serveWithContext api (genContext $ sessions state) $
    server state


run :: Int -> IO ()
run port = do
  state <- initialSessions
  Warp.run port $ app state
