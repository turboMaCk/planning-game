{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Api.Error
  ( module AgilePoker.Api.Error.Class
  ) where

import           AgilePoker.Api.Authorization.Type (AuthorizationError (..))
import           AgilePoker.Api.Error.Class        (Error (..), ErrorType (..),
                                                    respondError)
import           AgilePoker.Data                   (GameError (..),
                                                    SessionError (..),
                                                    TableError (..))


instance Error AuthorizationError where
  toType SessionNotFound  = Forbidden
  toType SessionIdMissing = Unauthorized

  toReadable SessionNotFound  = "Session expired."
  toReadable SessionIdMissing = "Session required."


instance Error SessionError where
  toType SessionDoesNotExist     = NotFound
  toReadable SessionDoesNotExist = "Session doesn't exist."


instance Error TableError where
  toType TableNotFound              = NotFound
  toType NameTaken                  = Conflict
  toType PlayerNotFound             = Forbidden
  toType (GameError GameFinished)   = Forbidden
  toType (GameError VotingEndedErr) = Forbidden

  toReadable TableNotFound              = "Table doesn't exist."
  toReadable NameTaken                  = "Name is already taken."
  toReadable PlayerNotFound             = "You're not a player on this table."
  toReadable (GameError GameFinished)   = "Game is already finished."
  toReadable (GameError VotingEndedErr) = "Voting is already closed."
