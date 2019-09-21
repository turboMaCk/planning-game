module Main where

import           Control.Concurrent            (forkIO)
import           System.Environment            (lookupEnv)

import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Middleware.Static as Static

import qualified PlanningGame.Api              as Api
import qualified PlanningGame.GarbageCollector as GC


envRead :: Read a => String -> a -> IO a
envRead name def =
  maybe def read <$> lookupEnv name


main :: IO ()
main = do
  state <- Api.init
  gcPeriod <- envRead "GC_EVERY_MIN" 30
  gcTableMinLife <- envRead "GC_TABLE_MIN_LIFE_MIN" 120
  cachingStrategy <- Static.initCaching Static.PublicStaticCaching

  -- Start garbage collector
  _ <- forkIO $ GC.start state gcPeriod gcTableMinLife

  Warp.runEnv 3000 $ Api.app cachingStrategy state
