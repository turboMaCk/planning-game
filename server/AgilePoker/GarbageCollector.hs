module AgilePoker.GarbageCollector where

-- This implements process that periodically cleans
-- old data from ServerState to maitain certain levels
-- of space usage.


import           Control.Concurrent (MVar)
import           Control.Monad
import           Data.Map.Strict    (Map)

import qualified Control.Concurrent as Concurrent
import qualified Data.Map           as Map

import           AgilePoker.Data    (Tables, tableActive)


filterMaybes :: Ord k => Map k (Maybe a) -> Map k a
filterMaybes map =
  Map.foldrWithKey filter Map.empty map

  where
    filter :: Ord k => k -> Maybe a -> Map k a -> Map k a
    filter _ Nothing acc  = acc
    filter k (Just a) acc = Map.insert k a acc


putTables :: MVar Tables -> IO ()
putTables state = do
  ts <- Concurrent.readMVar state
  let ids = fst <$> Map.toList ts

  putStrLn $ "Tables ids: " <> show ids


gcTables :: MVar Tables -> IO ()
gcTables state =
  Concurrent.modifyMVar_ state $ \tables -> do
    labeled <- forM tables $ \s -> do
      table <- Concurrent.readMVar s

      if tableActive table then
        pure $ Just s
      else
        pure $ Nothing

    pure $ filterMaybes labeled


start :: Int -> MVar Tables -> IO ()
start t state = do
  putStrLn $ "GC will run every " <> show t <> "th minute."

  forever $ do
    Concurrent.threadDelay (10^6 * 60 * t )

    putStrLn "Garbage collectiong...."
    putTables state

    gcTables state

    putTables state
