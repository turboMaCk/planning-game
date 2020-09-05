{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module PlanningGame.Api
  ( ServerState
  , State.init
  , app
  ) where

import           Control.Concurrent              (MVar)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Data.Text                       (Text)
import           Network.Wai.Middleware.Static   (CacheContainer)
import           Servant
import           Servant.API.WebSocket           (WebSocket)

import qualified Control.Concurrent              as Concurrent
import qualified Network.WebSockets              as WS

import           PlanningGame.Api.Authorization
import           PlanningGame.Api.PlayerInfo

import           PlanningGame.Data
import           PlanningGame.Data.AutoIncrement (WithId)
import           PlanningGame.State              (ServerState)

import qualified PlanningGame.Api.Error          as Error
import qualified PlanningGame.Api.Middleware     as Middleware
import qualified PlanningGame.Data.Session       as Session
import qualified PlanningGame.Data.Table         as Table
import qualified PlanningGame.Data.Table.Stream  as TableStream
import qualified PlanningGame.State              as State


-- API


type Api = "status"                                :> Get  '[JSON] Text
      :<|> "session"                               :> Post '[JSON] SessionJSON
      :<|> "session" :> AuthProtect "header"       :> Get  '[JSON] SessionJSON
      :<|> "tables"  :> AuthProtect "header"       :> ReqBody '[JSON] PlayerInfo      :> Post '[JSON] Table
      :<|> "tables"  :> AuthProtect "header"       :> Capture "tableid" (Id Table)  :> "join"
                     :> ReqBody '[JSON] PlayerInfo :> Post '[JSON] Table
      :<|> "tables"  :> AuthProtect "header"       :> Capture "tableid" (Id Table)  :> "me"
                                                   :> Get '[JSON] (WithId Player)
      :<|> "tables"  :> AuthProtect "cookie"       :> Capture "tableid" (Id Table)  :> "stream" :> WebSocket


api :: Proxy Api
api = Proxy


-- Server


genContext :: MVar Sessions -> Context (SessionHeaderAuth : SessionCookieAuth ': '[])
genContext state =
  authHeaderHandler state :. authCookieHandler state :. EmptyContext


server :: ServerState -> Server Api
server state = statusApi
           :<|> createSession
           :<|> getSession
           :<|> createTableHandler
           :<|> joinTableHandler
           :<|> meHandler
           :<|> streamTableHandler

  where
    statusApi :: Handler Text
    statusApi = pure "OK"

    createSession :: Handler SessionJSON
    createSession =
      SessionJSON <$>
        liftIO (Concurrent.modifyMVar (State.sessions state) Session.add)

    getSession :: HeaderAuth Session -> Handler SessionJSON
    getSession =
      pure . SessionJSON . unHeaderAuth

    createTableHandler :: HeaderAuth Session -> PlayerInfo -> Handler Table
    createTableHandler (HeaderAuth session) PlayerInfo { name, status } = do
      res <- liftIO $ Concurrent.modifyMVar (State.tables state)
                $ Table.create session name status

      either Error.respond pure res

    joinTableHandler :: HeaderAuth Session -> Id Table -> PlayerInfo -> Handler Table
    joinTableHandler (HeaderAuth session) id' PlayerInfo { name, status } = do
      tables <- liftIO $ Concurrent.readMVar (State.tables state)
      tableRes <- liftIO $ TableStream.join session id' name status tables

      either Error.respond pure tableRes

    meHandler :: HeaderAuth Session -> Id Table -> Handler (WithId Player)
    meHandler (HeaderAuth session) tableId = do
      ts <- liftIO $ Concurrent.readMVar (State.tables state)
      playerRes <- liftIO $ Table.getPlayer session tableId ts

      either Error.respond pure playerRes

    streamTableHandler :: MonadIO m => CookieAuth Session -> Id Table -> WS.Connection -> m ()
    streamTableHandler (CookieAuth session) id' conn =
      liftIO $ TableStream.handler (State.tables state) session id' conn


app :: CacheContainer -> ServerState -> Application
app caching state = Middleware.static caching $
    serveWithContext api (genContext $ State.sessions state) $
    server state
