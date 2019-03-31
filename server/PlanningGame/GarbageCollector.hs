{-# LANGUAGE NamedFieldPuns #-}

module PlanningGame.GarbageCollector (start) where

-- This implements process that periodically cleans
-- old data from ServerState to maitain certain levels
-- of space usage.

import           Control.Concurrent (MVar)
import           Control.Monad
import           Data.Map.Strict    (Map)
import           Data.Time.Clock    (UTCTime)

import qualified Control.Concurrent as Concurrent
import qualified Data.Map           as Map
import qualified Data.Time.Clock    as Clock

import           PlanningGame.Data.Table    (Tables)
import           PlanningGame.State

import qualified PlanningGame.Data.Table as Table


filterMaybes :: Ord k => Map k (Maybe a) -> Map k a
filterMaybes map' =
  Map.foldrWithKey filter' Map.empty map'

  where
    filter' :: Ord k => k -> Maybe a -> Map k a -> Map k a
    filter' _ Nothing acc  = acc
    filter' k (Just a) acc = Map.insert k a acc


gcTables :: MVar Tables -> Integer -> UTCTime -> IO ()
gcTables state minDeltaMin now =
  Concurrent.modifyMVar_ state $ \tables -> do
    labeled <- forM tables $ \s -> do
      table <- Concurrent.readMVar s

      let nominalDiffTime =
            Clock.diffUTCTime now $ Table.createdAt table

      if (nominalDiffTime < fromInteger (60 * minDeltaMin)) || Table.isActive table then
        pure $ Just s

      else
        pure $ Nothing

    pure $ filterMaybes labeled


start :: ServerState -> Int -> Integer -> IO ()
start (ServerState { tables }) frequency tableMinLife = do
  putStrLn $ "GC will run every " <> show frequency <> "th minute."

  forever $ do
    -- convert minutes
    Concurrent.threadDelay (10^(6 :: Int) * 60 * frequency)

    putStrLn "Garbage collecting...."

    now <- Clock.getCurrentTime
    gcTables tables tableMinLife now
