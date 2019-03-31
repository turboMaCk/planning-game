module PlanningGame.State (ServerState(..), initState) where


import           Control.Concurrent  (MVar)

import qualified Control.Concurrent as Concurrent

import           PlanningGame.Data    (Sessions, Tables, emptySessions)
import qualified PlanningGame.Data.Table as Table


data ServerState = ServerState
  { sessions :: MVar Sessions
  , tables   :: MVar Tables
  }


initState :: IO ServerState
initState = ServerState
  <$> Concurrent.newMVar emptySessions
  <*> Concurrent.newMVar Table.empty
