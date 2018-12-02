{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Data.Table.Event
  ( Event(..)
  , encodeEvent
  ) where

import Data.ByteString (ByteString)
import Data.Aeson.Types (ToJSON(..), (.=))
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.IntMap as IntMap
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AT

import AgilePoker.Data.Player (Player)

import AgilePoker.Data.Table.Type (Table)


data Event
    = PlayerJoined Player
    | PlayerStatusUpdate Player
    | SyncTableState Table


instance ToJSON Event where
  toJSON (PlayerJoined player) =
    AT.object
        [ "event" .= T.pack "UserJoined"
        , "player" .= player
        ]
  toJSON (PlayerStatusUpdate player) =
    AT.object
        [ "event" .= T.pack "UserStatusUpdate"
        , "player" .= player
        ]
  toJSON (SyncTableState table) =
    AT.object
        [ "event" .= T.pack "SyncTableState"
        , "table" .= table
        ]


encodeEvent :: Event -> ByteString
encodeEvent = LB.toStrict . Aeson.encode
