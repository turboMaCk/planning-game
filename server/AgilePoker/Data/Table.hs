{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module AgilePoker.Data.Table
  (Table(..), Tables, TableId, Event, TableError(..)
  , emptyTables, createTable, joinTable
  , getTablePlayer
  , tableStreamHandler
  ) where

import Data.Maybe (fromMaybe)
import Control.Monad (forM_, forever, mzero)
import Control.Concurrent (MVar)
import Control.Exception (finally)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Control.Concurrent as Concurrent

import AgilePoker.Data.Id
import AgilePoker.Data.Session
import AgilePoker.Data.Player

import AgilePoker.Data.Table.Event
import AgilePoker.Data.Table.Type


createTable :: Session -> T.Text -> Tables -> IO ( Tables, Table )
createTable Session { sessionId=id' } name tables = do
  tId <- generateId tables
  let banker = createPlayer name
  let newTable = Table tId ( id', banker ) emptyPlayers
  mvarTable <- Concurrent.newMVar newTable
  pure $ ( Map.insert tId mvarTable tables
         , newTable
         )


-- @TODO: Add check if session is not already present
joinTable :: Session -> TableId -> T.Text -> Tables -> IO ( Either TableError Table )
joinTable session@Session { sessionId=id' } tableId name tables =
  case Map.lookup tableId tables of
    Just mvar -> do
      table <- Concurrent.readMVar mvar
      if playerName (snd $ tableBanker table) == name then
        pure $ Left NameTaken
      else
        let
            mPlayers = addPlayer session name (tablePlayers table)
        in
        case mPlayers of
          Just (newPlayers, newPlayer) ->
            Concurrent.modifyMVar mvar $ \t -> do
                let updatedTable = t { tablePlayers = newPlayers }

                -- Broadcast to connections
                broadcast t $ PlayerJoined newPlayer

                pure ( updatedTable, Right updatedTable )
          Nothing ->
             pure $ Left NameTaken

    Nothing ->
      pure $ Left TableNotFound

  where
    updateTables :: Table -> Players -> IO ( Tables, Maybe Table )
    updateTables table players =
       undefined


getTablePlayer :: Session -> TableId -> Tables -> IO (Either TableError Player)
getTablePlayer Session { sessionId=sId } tableId tables =
  fromMaybe (pure $ Left TableNotFound) $ getPlayer' <$>
    Map.lookup tableId tables

  where
    getPlayer' :: MVar Table -> IO (Either TableError Player)
    getPlayer' mvar = do
      table <- Concurrent.readMVar mvar

      if fst (tableBanker table) == sId then
          pure $ Right $ snd (tableBanker table)
      else
          pure $ maybe (Left PlayerNotFound) Right $
            Map.lookup sId $ tablePlayers table


allConnections :: Table -> [ WS.Connection ]
allConnections Table { tableBanker=banker, tablePlayers=players } =
  concat $ (allPlayerConnections $ snd banker)
         : (foldr (\p acc -> allPlayerConnections p : acc) [] players)


assignConnection :: SessionId -> WS.Connection -> Table -> ( Table, Maybe ( Player, Int ) )
assignConnection sId conn table@Table { tableBanker=banker, tablePlayers=players } =
    if fst banker == sId then
        let ( updatedBanker, connId ) = addConnectionToPlayer conn $ snd banker
        in
        ( table { tableBanker = ( sId, updatedBanker ) }
        , Just ( updatedBanker, connId )
        )
    else
        let ( updatedPlayers, mPair ) = addPlayerConnection sId conn players
        in
        case mPair of
            Nothing     -> ( table, Nothing )
            Just pair -> ( table { tablePlayers = updatedPlayers }
                           , Just pair
                           )


-- @TODO: implement
handleStreamMsg :: WS.Connection -> IO ()
handleStreamMsg conn = forever $ do
  msg :: ByteString <- WS.receiveData conn
  pure ()


disconnect :: MVar Table -> SessionId -> Int -> IO ()
disconnect state sessionId connId =
  Concurrent.modifyMVar_ state $ \table@Table { tableBanker=banker, tablePlayers=players } ->
    if fst banker == sessionId then do
      let updatedTable = table { tableBanker = ( fst banker , removeConnectionFromPlayer connId $ snd banker ) }
      let player = snd $ tableBanker updatedTable

      if hasConnection player then
        pure ()
      else
        broadcast updatedTable $ PlayerStatusUpdate player

      pure updatedTable
    else do
      let updatedTable = table { tablePlayers = disconnectPlayer sessionId connId (tablePlayers table) }
      let mPlayer = Map.lookup sessionId $ tablePlayers updatedTable

      maybe mzero (broadcast updatedTable . PlayerStatusUpdate) mPlayer
      pure updatedTable


tableStreamHandler :: MVar Tables -> Session -> TableId -> WS.Connection -> IO ()
tableStreamHandler state Session { sessionId=sId } id' conn = do
  tables <- Concurrent.readMVar state
  let mTable = Map.lookup id' tables

  case mTable of
    Just tableState -> do

        -- 1. Assign connection to player
        mConnId <- Concurrent.modifyMVar tableState $ pure . assignConnection sId conn

        -- 2. Sync sate to new player
        table <- Concurrent.readMVar tableState
        WS.sendTextData conn $ encodeEvent $ SyncTableState table

        -- 3. Start player handler
        case mConnId of
            Just ( player, connId ) ->

                -- 3.1 Remove connection on disconnection
                flip finally (disconnect tableState sId connId) $ do

                    -- 3.2 Ping Thread
                    WS.forkPingThread conn 30

                    -- 3.3 Broadcast join event
                    if playerNumberOfConnections player == 1 then do
                        table <- Concurrent.readMVar tableState
                        broadcast table $ PlayerStatusUpdate player
                    else
                        mzero

                    -- 3.4 Delegate to Msg handler
                    handleStreamMsg conn

            Nothing -> do
                -- @TODO: player doesn't exist (session is not a member)
                putStrLn "User not a member"
                mzero

    Nothing -> do
        -- @TODO: handle table doesn't exist
        putStrLn "Table not found"
        mzero


broadcast :: Table -> Event -> IO ()
broadcast table event = do
  forM_ (allConnections table) $ flip WS.sendTextData $ encodeEvent event
