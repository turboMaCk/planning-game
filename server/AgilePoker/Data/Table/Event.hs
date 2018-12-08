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
import AgilePoker.Data.Game (RunningGame, Vote)

import AgilePoker.Data.Table.Type (Table)


data Event
    = PlayerJoined Player
    | PlayerStatusUpdate Player
    | SyncTableState Table
    | GameStarted RunningGame -- @TODO: add points so far
    | VoteAccepted Player
    | VotingEnded RunningGame
    | GameEnded -- @TODO: add history overview


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
  toJSON (GameStarted game) =
    AT.object
        [ "event" .= T.pack "GameStarted"
        , "game"  .= game
        ]
  toJSON (VoteAccepted player) =
    AT.object
        [ "event"  .= T.pack "VoteAccepted"
        , "player" .= player
        ]
  toJSON (VotingEnded game) =
    AT.object
        [ "event"  .= T.pack "VotingEnded"
        , "game"   .= game
        ]
  toJSON GameEnded =
    AT.object
        [ "event"  .= T.pack "GameEnded"
        ]


encodeEvent :: Event -> ByteString
encodeEvent = LB.toStrict . Aeson.encode
