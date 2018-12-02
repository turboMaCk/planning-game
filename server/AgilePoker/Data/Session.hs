{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Data.Session
  ( Session(..), SessionId, Sessions, SessionError(..)
  , emptySessions, addSession, getSession, removeSession
  ) where

import Data.ByteString (ByteString)
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Aeson.Types (ToJSON(..), (.=))
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson.Types as AT


import AgilePoker.Data.Id


data Session = Session
  { sessionId :: ByteString
  }


instance ToJSON Session where
  toJSON (Session { sessionId=id }) =
    AT.object
        [ "id" .= TE.decodeUtf8 id
        ]


-- @TODO: Just alias for now
type SessionId = ByteString


type Sessions =
  Map ByteString Session


data SessionError
  = SessionDoesNotExist


addSession :: Sessions -> IO ( Sessions, ( SessionId, Session ) )
addSession sessions = do
    newId <- generateId sessions
    let newSession = Session newId
    pure $ ( Map.insert newId newSession sessions
           , ( newId, newSession )
           )


getSession :: ByteString -> Sessions -> Maybe Session
getSession = Map.lookup


emptySessions :: Sessions
emptySessions = Map.empty


removeSession :: SessionId -> Sessions -> Sessions
removeSession = Map.delete
