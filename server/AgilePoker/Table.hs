{-# LANGUAGE OverloadedStrings     #-}

module AgilePoker.Table
  (Table(..), Tables, TableId
  , emptyTables, createTable, joinTable
  , tableStreamHandler
  ) where

import Data.ByteString (ByteString)
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
joinTable :: Session -> TableId -> T.Text -> Tables -> IO ( Maybe Table )
joinTable session@Session { sessionId=id' } tableId name tables =
  case Map.lookup tableId tables of
    Just mvar -> do
      table <- Concurrent.readMVar mvar
      if playerName (snd $ tableBanker table) == name then
        pure Nothing
      else
        let
            mPlayers = addPlayer session name (tablePlayers table)
        in
        case mPlayers of
          Just newPlayers ->
            Concurrent.modifyMVar mvar $ \t -> do
               let updatedTable = t { tablePlayers = newPlayers }
               pure ( updatedTable, Just updatedTable )
          Nothing ->
             pure Nothing

    Nothing ->
      pure Nothing

  where
    updateTables :: Table -> Players -> IO ( Tables, Maybe Table )
    updateTables table players =
       undefined


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
