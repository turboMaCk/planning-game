module Main where

import           Control.Concurrent          (forkIO)
import           Data.Maybe                  (fromMaybe)
import           System.Environment          (lookupEnv)

import qualified Network.Wai.Handler.Warp    as Warp

import qualified AgilePoker.Api              as Api
import qualified AgilePoker.GarbageCollector as GC


envRead :: Read a => String -> a -> IO a
envRead name def =
  maybe def read <$> lookupEnv name


main :: IO ()
main = do
  state <- Api.initState
  gcPeriod <- envRead "GC_EVERY_MIN" 30
  gcTableMinLife <- envRead "GC_TABLE_MIN_LIFE_MIN" 120

  -- Start garbage collector
  forkIO $ GC.start state gcPeriod gcTableMinLife

  Warp.runEnv 3000 $ Api.app state
