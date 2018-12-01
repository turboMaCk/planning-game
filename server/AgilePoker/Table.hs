{-# LANGUAGE OverloadedStrings     #-}

module AgilePoker.Table
  (Table(..), Tables, TableId
  , emptyTables, createTable, joinTable
  , getTablePlayer
  , tableStreamHandler
  ) where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (ToJSON(..), (.=), object)
import Control.Concurrent (MVar)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.WebSockets as WS
import qualified Control.Concurrent as Concurrent

import AgilePoker.Id
import AgilePoker.Session
import AgilePoker.Player
import AgilePoker.Api.Errors


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
          Just newPlayers ->
            Concurrent.modifyMVar mvar $ \t -> do
               let updatedTable = t { tablePlayers = newPlayers }
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



-- @TODO: Implement
tableStreamHandler :: MVar Tables -> Session -> TableId -> WS.Connection -> IO ()
tableStreamHandler state Session { sessionId=sId } id' conn =
  -- 1. Assign connection to player
  -- 2. Sync sate to new player
  -- 3. Start player handler
  -- 3.1 Remove connection on disconnection
  -- 3.2 Ping Thread
  -- 3.3 Broadcast join event
  -- 3.4 Delegate to Msg handler

  pure ()
