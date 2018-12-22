{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Data.Session
  ( Session(..)
  , SessionId
  , Sessions
  , SessionError(..)
  , emptySessions
  , addSession
  , getSession
  , removeSession
  ) where

import           Data.Aeson.Types   (ToJSON (..), (.=))
import qualified Data.Aeson.Types   as AT
import           Data.ByteString    (ByteString)
import qualified Data.IntMap        as IntMap
import           Data.IntMap.Strict (IntMap)
import qualified Data.Map           as Map
import           Data.Map.Strict    (Map)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE


import           AgilePoker.Data.Id (Id, generateId)


data Session = Session
  { sessionId :: Id SessionId
  }


instance ToJSON Session where
  toJSON (Session { sessionId=id }) =
    AT.object
        [ "id" .= id
        ]


data SessionId


type Sessions =
  Map (Id SessionId) Session


data SessionError
  = SessionDoesNotExist


addSession :: Sessions -> IO ( Sessions, ( Id SessionId, Session ) )
addSession sessions = do
    newId <- generateId sessions
    let newSession = Session newId
    pure $ ( Map.insert newId newSession sessions
           , ( newId, newSession )
           )


getSession :: Id SessionId -> Sessions -> Maybe Session
getSession = Map.lookup


emptySessions :: Sessions
emptySessions = Map.empty


removeSession :: Id SessionId -> Sessions -> Sessions
removeSession = Map.delete
