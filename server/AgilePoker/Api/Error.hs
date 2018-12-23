{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Api.Error
  ( module AgilePoker.Api.Error.Class
  ) where

import           AgilePoker.Api.Authorization.Type (AuthorizationError (..))
import           AgilePoker.Api.Error.Class        (Error (..), ErrorType (..),
                                                    respondError)
import           AgilePoker.Data                   (GameError (..),
                                                    PlayerError (..),
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


instance Error PlayerError where
  toType NameTaken = Conflict
  toType NameEmpty = Unprocessable

  toReadable NameTaken = "Player with this name already exists."
  toReadable NameEmpty = "Name can't be empty."


instance Error GameError where
  toType GameFinished   = Forbidden
  toType VotingEndedErr = Forbidden

  toReadable GameFinished   = "Game is already finished."
  toReadable VotingEndedErr = "Votting is already closed."


instance Error TableError where
  toType TableNotFound   = NotFound
  toType PlayerNotFound  = Forbidden
  toType (GameError e)   = toType e
  toType (PlayerError e) = toType e

  toReadable TableNotFound   = "Table doesn't exist."
  toReadable PlayerNotFound  = "You're not a player on this table."
  toReadable (GameError e)   = toReadable e
  toReadable (PlayerError e) = toReadable e
