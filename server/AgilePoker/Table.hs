{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module AgilePoker.Table
  (Table(..), Tables, TableId
  , emptyTables, createTable, joinTable
  , getTablePlayer
  , tableStreamHandler
  ) where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (ToJSON(..), (.=), object)
import Control.Monad (forM_, forever, mzero)
import Control.Concurrent (MVar)
import Control.Exception (finally)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.WebSockets as WS
import qualified Control.Concurrent as Concurrent

import AgilePoker.Id
import AgilePoker.Session
import AgilePoker.Player
import AgilePoker.Api.Errors
import AgilePoker.Event


-- @TODO: Just alias for now
type TableId = ByteString


-- @TODO: incomplete (missing games)
data Table = Table
  { tableId :: TableId
  , tableBanker :: ( SessionId, Player )
  , tablePlayers :: Players
  }

instance ToJSON Table where
  toJSON table =
    object
        [ "id" .= TE.decodeUtf8 (tableId table)
        , "banker" .= snd (tableBanker table)
        , "players" .= fmap snd (Map.toList $ tablePlayers table)
        ]


type Tables =
  Map.Map ByteString (MVar Table)


data TableError
  = TableNotFound
  | NameTaken
  | PlayerNotFound


instance Error TableError where
  toType TableNotFound  = NotFound
  toType NameTaken      = Conflict
  toType PlayerNotFound = Forbidden

  toReadable TableNotFound  = "Table doesn't exist"
  toReadable NameTaken      = "Name is already taken"
  toReadable PlayerNotFound = "You're not a player on this table"


emptyTables :: Tables
emptyTables = Map.empty


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


assignConnection :: SessionId -> WS.Connection -> Table -> ( Table, Maybe Int )
assignConnection sId conn table@Table { tableBanker=banker, tablePlayers=players } =
    if fst banker == sId then
        let ( updatedBanker, connId ) = addConnectionToPlayer conn $ snd banker
        in
        ( table { tableBanker = ( sId, updatedBanker ) }
        , Just connId
        )
    else
        let ( updatedPlayers, mConnId ) = addPlayerConnection sId conn players
        in
        case mConnId of
            Nothing     -> ( table, Nothing )
            Just connId -> ( table { tablePlayers = updatedPlayers }
                           , Just connId)


-- @TODO: implement
handleStreamMsg :: WS.Connection -> IO ()
handleStreamMsg conn = forever $ do
  msg :: ByteString <- WS.receiveData conn
  pure ()


-- @TODO: check if user state changed & broadcast
disconnect :: MVar Table -> SessionId -> Int -> IO ()
disconnect state sessionId connId =
  Concurrent.modifyMVar_ state $ \table@Table { tableBanker=banker, tablePlayers=players } ->
    if fst banker == sessionId then
      pure $ table { tableBanker = ( fst banker , removeConnectionFromPlayer connId $ snd banker ) }
    else
      pure $ table { tablePlayers = disconnectPlayer sessionId connId (tablePlayers table) }


tableStreamHandler :: MVar Tables -> Session -> TableId -> WS.Connection -> IO ()
tableStreamHandler state Session { sessionId=sId } id' conn = do
  tables <- Concurrent.readMVar state
  let mTable = Map.lookup id' tables

  case mTable of
    Just tableState -> do

        -- 1. Assign connection to player
        mConnId <- Concurrent.modifyMVar tableState $ pure . assignConnection sId conn

        -- @TODO 2. Sync sate to new player

        -- 3. Start player handler
        case mConnId of
            Just connId ->

                -- 3.1 Remove connection on disconnection
                flip finally (disconnect tableState sId connId) $ do

                    -- 3.2 Ping Thread
                    WS.forkPingThread conn 30

                    -- @TODO 3.3 Broadcast join event
                    -- table <- Concurrent.readMVar tableState

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


wtf :: Table -> T.Text -> IO ()
wtf table t =
  forM_ (allConnections table) $ flip WS.sendTextData t


broadcast :: Table -> Event -> IO ()
broadcast table event = do
  forM_ (allConnections table) $ flip WS.sendTextData $ encodeEvent event
