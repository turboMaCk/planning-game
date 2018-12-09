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

import AgilePoker.Api.GameSnapshot (snapshot)
import AgilePoker.Data.Player (Player, Players)
import AgilePoker.Data.Game (Games)

import AgilePoker.Data.Table.Type (Table)


data Event
    = PlayerJoined Player
    | PlayerStatusUpdate Player
    | SyncTableState Table
    | GameStarted Players Games
    | VoteAccepted Player
    | VotingEnded Players Games
    | GameEnded Players Games


instance ToJSON Event where
  toJSON (PlayerJoined player) =
    AT.object
        [ "event"  .= T.pack "UserJoined"
        , "player" .= player
        ]
  toJSON (PlayerStatusUpdate player) =
    AT.object
        [ "event"  .= T.pack "UserStatusUpdate"
        , "player" .= player
        ]
  toJSON (SyncTableState table) =
    AT.object
        [ "event" .= T.pack "SyncTableState"
        , "table" .= table
        ]
  toJSON (GameStarted players games) =
    AT.object
        [ "event" .= T.pack "GameStarted"
        , "game"  .= snapshot players games
        ]
  toJSON (VoteAccepted player) =
    AT.object
        [ "event"  .= T.pack "VoteAccepted"
        , "player" .= player
        ]
  toJSON (VotingEnded players games) =
    AT.object
        [ "event" .= T.pack "VotingEnded"
        , "game"  .= snapshot players games
        ]
  toJSON (GameEnded players games) =
    AT.object
        [ "event" .= T.pack "GameEnded"
        , "game"  .= snapshot players games
        ]


encodeEvent :: Event -> ByteString
encodeEvent = LB.toStrict . Aeson.encode
