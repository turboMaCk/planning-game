{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module AgilePoker.Api (ServerState, initState, app) where

import           Control.Concurrent           (MVar)
import qualified Control.Concurrent           as Concurrent
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.ByteString              (ByteString)
import           Data.Maybe                   (maybe)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Data.Text.IO                 as T
import           Network.Wai                  (Response)
import qualified Network.WebSockets           as WS
import           Servant
import           Servant.API.WebSocket        (WebSocket)

import           AgilePoker.Api.Authorization
import           AgilePoker.Api.Error
import           AgilePoker.Api.Middleware
import           AgilePoker.Api.UserInfo

import           AgilePoker.Data


-- State


data ServerState = ServerState
  { sessions :: MVar Sessions
  , tables   :: MVar Tables
  }


initState :: IO ServerState
initState = ServerState
  <$> Concurrent.newMVar emptySessions
  <*> Concurrent.newMVar emptyTables


-- API


type Api = "status"                                :> Get  '[JSON] T.Text
      :<|> "session"                               :> Post '[JSON] SessionJSON
      :<|> "session" :> AuthProtect "header"       :> Get  '[JSON] SessionJSON
      :<|> "tables"  :> AuthProtect "header"       :> ReqBody '[JSON] UserInfo        :> Post '[JSON] Table
      :<|> "tables"  :> AuthProtect "header"       :> Capture "tableid" (Id TableId)  :> "join"
                     :> ReqBody '[JSON] UserInfo   :> Post '[JSON] Table
      :<|> "tables"  :> AuthProtect "header"       :> Capture "tableid" (Id TableId)  :> "me"
                                                   :> Get '[JSON] Player
      :<|> "tables"  :> AuthProtect "cookie"       :> Capture "tableid" (Id TableId)  :> "stream" :> WebSocket


api :: Proxy Api
api = Proxy


-- Server


genContext :: MVar Sessions -> Context (SessionHeaderAuth : SessionCookieAuth ': '[])
genContext state =
  authHeaderHandler state :. authCookieHandler state :. EmptyContext


server :: ServerState -> Server Api
server state = status
           :<|> createSession
           :<|> getSession
           :<|> createTableHandler
           :<|> joinTableHandler
           :<|> meHandler
           :<|> streamTableHandler

  where
    status :: Handler T.Text
    status = pure "OK"

    createSession :: Handler SessionJSON
    createSession =
      pure . SessionJSON =<<
        (liftIO $ Concurrent.modifyMVar (sessions state) addSession)

    getSession :: (HeaderAuth Session) -> Handler SessionJSON
    getSession =
      pure . SessionJSON . unHeaderAuth

    createTableHandler :: (HeaderAuth Session) -> UserInfo -> Handler Table
    createTableHandler (HeaderAuth session) UserInfo { userName=name } = do
      res <- liftIO $ Concurrent.modifyMVar (tables state)
                $ createTable session name

      either respondError pure res

    joinTableHandler :: (HeaderAuth Session) -> Id TableId -> UserInfo -> Handler Table
    joinTableHandler (HeaderAuth session) id' UserInfo { userName=name } = do
      tables <- liftIO $ Concurrent.readMVar (tables state)
      tableRes <- liftIO $ joinTable session id' name tables

      either respondError pure tableRes

    meHandler :: (HeaderAuth Session) -> Id TableId -> Handler Player
    meHandler (HeaderAuth session) tableId = do
      ts <- liftIO $ Concurrent.readMVar (tables state)
      playerRes <- liftIO $ getTablePlayer session tableId ts

      either respondError pure playerRes

    streamTableHandler :: MonadIO m => (CookieAuth Session) -> Id TableId -> WS.Connection -> m ()
    streamTableHandler (CookieAuth session) id' conn =
      liftIO $ tableStreamHandler (tables state) session id' conn


app :: ServerState -> Application
app state = staticMiddleware $
    serveWithContext api (genContext $ sessions state) $
    server state
