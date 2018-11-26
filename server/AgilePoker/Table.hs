{-# LANGUAGE OverloadedStrings     #-}

module AgilePoker.Table
  (Table(..), Tables, TableId
  , emptyTables, createTable
  ) where

import Data.ByteString (ByteString)
import Data.Aeson.Types (ToJSON(..), (.=), object)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

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
        [ "id" .= tableId table
        , "banker" .= toJSON (tableBanker table)
        , "players" .= toJSON (tablePlayers table)
        ]


type Tables = Map.Map ByteString Table


emptyTables :: Tables
emptyTables = Map.empty


createTable :: Session -> T.Text -> Tables -> IO ( Tables, Table )
createTable Session { sessionId=id' } name tables = do
  newId <- generateId tables
  let banker = createPlayer name
  let newTable = Table ( id', banker ) emptyPlayers
  pure $ ( Map.insert newId newTable tables
         , newTable
         )


-- joinTable :: Session -> TableId -> T.Text -> Tables -> IO ( Tables, Maybe Table )
-- joinTable session@Session { sessionId=id' } tableId name tables =
--   case Map.lookip tableId tables of
--     Just (table@Table { tableBanker=banker, tablePlayers=players }) ->
--       if playerNamer banker == name then
--         pure ( tables, Nothing )
--       else
--         case addPlayer session name players of
--           ( players )
--         pure ( table { tablePlayers =  } )
