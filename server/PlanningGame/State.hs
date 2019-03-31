module PlanningGame.State (ServerState(..), init) where


import           Prelude      hiding (init)
import           Control.Concurrent  (MVar)

import qualified Control.Concurrent as Concurrent

import           PlanningGame.Data    (Sessions, Tables)

import qualified PlanningGame.Data.Table as Table
import qualified PlanningGame.Data.Session as Session


data ServerState = ServerState
  { sessions :: MVar Sessions
  , tables   :: MVar Tables
  }


init :: IO ServerState
init = ServerState
  <$> Concurrent.newMVar Session.empty
  <*> Concurrent.newMVar Table.empty
