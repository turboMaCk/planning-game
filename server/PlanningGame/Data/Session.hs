{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PlanningGame.Data.Session
  ( Session
  , SessionId
  , Sessions
  , SessionError(..)
  , SessionJSON(..)
  , emptySessions
  , addSession
  , getSession
  , removeSession
  ) where

import           Data.Aeson.Types   (ToJSON (..), object, (.=))
import           Data.Set           (Set)

import qualified Data.Set           as Set

import           PlanningGame.Api.Error.Class        (Error (..),
                                                      ErrorType (..))
import           PlanningGame.Data.Id (Id, generateId)


data SessionId


type Session =
  Id SessionId


type Sessions =
  Set (Id SessionId)


data SessionError
  = SessionDoesNotExist
  deriving (Show, Eq)


instance Error SessionError where
  toType SessionDoesNotExist     = NotFound
  toReadable SessionDoesNotExist = "Session doesn't exist."


newtype SessionJSON =
  SessionJSON { unSessionJSON :: Session }


instance ToJSON SessionJSON where
  toJSON id' =
    object
        [ "id" .= unSessionJSON id'
        ]


addSession :: Sessions -> IO ( Sessions, Id SessionId )
addSession sessions = do
    newId <- generateId sessions
    pure $ ( Set.insert newId sessions, newId )


emptySessions :: Sessions
emptySessions = Set.empty


removeSession :: Id SessionId -> Sessions -> Sessions
removeSession = Set.delete


getSession :: Session -> Sessions -> Maybe Session
getSession session sessions =
  if Set.member session sessions then
    Just session
  else
    Nothing
