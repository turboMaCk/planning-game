module Main where

import           Control.Concurrent          (forkIO)

import qualified Network.Wai.Handler.Warp    as Warp

import qualified AgilePoker.Api              as Api
import qualified AgilePoker.GarbageCollector as GC


main :: IO ()
main = do
  state <- Api.initState

  -- Start garbage collector
  forkIO $ GC.start 1 (Api.tables state)

  Warp.runEnv 3000 $ Api.app state
