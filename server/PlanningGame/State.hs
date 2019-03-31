module PlanningGame.State (ServerState(..), initState) where


import           Control.Concurrent  (MVar)

import qualified Control.Concurrent as Concurrent

import           PlanningGame.Data    (Sessions, Tables)

import qualified PlanningGame.Data.Table as Table
import qualified PlanningGame.Data.Session as Session


data ServerState = ServerState
  { sessions :: MVar Sessions
  , tables   :: MVar Tables
  }


initState :: IO ServerState
initState = ServerState
  <$> Concurrent.newMVar Session.empty
  <*> Concurrent.newMVar Table.empty
