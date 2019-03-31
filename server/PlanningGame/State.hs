module PlanningGame.State (ServerState(..), init) where


import           Control.Concurrent        (MVar)
import           Prelude                   hiding (init)

import qualified Control.Concurrent        as Concurrent

import           PlanningGame.Data         (Sessions, Tables)

import qualified PlanningGame.Data.Session as Session
import qualified PlanningGame.Data.Table   as Table


data ServerState = ServerState
  { sessions :: MVar Sessions
  , tables   :: MVar Tables
  }


init :: IO ServerState
init = ServerState
  <$> Concurrent.newMVar Session.empty
  <*> Concurrent.newMVar Table.empty
