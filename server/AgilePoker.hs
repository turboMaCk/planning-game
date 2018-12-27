module Main where

import           Control.Concurrent          (forkIO)
import           Data.Maybe                  (fromMaybe)
import           System.Environment          (lookupEnv)

import qualified Network.Wai.Handler.Warp    as Warp

import qualified AgilePoker.Api              as Api
import qualified AgilePoker.GarbageCollector as GC


envStr :: String -> String -> IO String
envStr name def =
  fromMaybe def <$> lookupEnv name


envInt :: String -> Int -> IO Int
envInt name def =
  maybe def read <$> lookupEnv name


main :: IO ()
main = do
  state <- Api.initState
  gcPeriod <- envInt "GC_EVERY" 30

  -- Start garbage collector
  forkIO $ GC.start state gcPeriod

  Warp.runEnv 3000 $ Api.app state
