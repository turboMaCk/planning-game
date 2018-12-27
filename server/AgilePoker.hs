module Main where

import qualified AgilePoker.Api              as Api
import qualified AgilePoker.GarbageCollector as GC
import           Control.Concurrent          (forkIO)
import qualified Network.Wai.Handler.Warp    as Warp


main :: IO ()
main = do
  state <- Api.initState

  -- Start garbage collector
  forkIO $ GC.start 1 (Api.tables state)

  Warp.runEnv 3000 $ Api.app state
