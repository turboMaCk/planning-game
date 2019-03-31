{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PlanningGame.Data.Session
  ( Session
  , SessionId
  , Sessions
  , SessionError(..)
  , SessionJSON(..)
  , empty
  , add
  , get
  , remove
  ) where

import           Data.Aeson.Types       (ToJSON (..), object, (.=))
import           Data.Set               (Set)

import qualified Data.Set               as Set

import           PlanningGame.Api.Error (Error (..), ErrorType (..))
import           PlanningGame.Data.Id   (Id, generateId)


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


add :: Sessions -> IO ( Sessions, Id SessionId )
add sessions = do
    newId <- generateId sessions
    pure $ ( Set.insert newId sessions, newId )


empty :: Sessions
empty = Set.empty


remove :: Id SessionId -> Sessions -> Sessions
remove = Set.delete


get :: Session -> Sessions -> Maybe Session
get session sessions =
  if Set.member session sessions then
    Just session
  else
    Nothing
