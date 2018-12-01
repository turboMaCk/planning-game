{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Event
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

import AgilePoker.Session
import AgilePoker.Player


data Event
    = PlayerJoined Player
    | PlayerStatusUpdate Player


instance ToJSON Event where
  toJSON (PlayerJoined player) =
    AT.object
        [ "event" .= T.pack "UserJoined"
        , "player" .= toJSON player
        ]
  toJSON (PlayerStatusUpdate player) =
    AT.object
        [ "event" .= T.pack "UserStatusUpdate"
        , "player" .= toJSON player
        ]


encodeEvent :: Event -> ByteString
encodeEvent = LB.toStrict . Aeson.encode
