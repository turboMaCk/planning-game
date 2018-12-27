module AgilePoker.State (ServerState(..), initState) where


import           Control.Concurrent  (MVar)

import qualified Control.Concurrent as Concurrent

import           AgilePoker.Data    (Sessions, Tables, emptySessions,
                                     emptyTables)


data ServerState = ServerState
  { sessions :: MVar Sessions
  , tables   :: MVar Tables
  }


initState :: IO ServerState
initState = ServerState
  <$> Concurrent.newMVar emptySessions
  <*> Concurrent.newMVar emptyTables
