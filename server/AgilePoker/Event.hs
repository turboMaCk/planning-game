{-# LANGUAGE OverloadedStrings #-}
module AgilePoker.Event
  ( Event
  , encodeEvent
  , userJoined
  , userLeft
  ) where

import Data.ByteString (ByteString)
import Data.Aeson.Types (ToJSON(..), (.=))
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.IntMap as IntMap
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AT

import AgilePoker.Session

data Event
    = UserJoined Session
    | UserLeft T.Text


instance ToJSON Event where
  toJSON (UserJoined (Session { sessionName=name, sessionConnections=conns })) =
    AT.object
        [ "event" .= T.pack "UserJoined"
        , "name" .= name
        , "connected" .= not (IntMap.null conns)
        ]
  toJSON (UserLeft name) =
    AT.object
        [ "event" .= T.pack "UserLeft"
        , "name" .= name
        ]


userJoined :: Session -> Event
userJoined = UserJoined


userLeft :: Session -> Event
userLeft Session { sessionName=name } =
  UserLeft name


encodeEvent :: Event -> ByteString
encodeEvent = LB.toStrict . Aeson.encode
