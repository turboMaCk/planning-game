{-# LANGUAGE OverloadedStrings     #-}

module AgilePoker.Api.Error (Error(..), ErrorType(..), respondError) where

import AgilePoker.Api.Error.Class (Error(..), respondError, ErrorType(..))
import AgilePoker.Data (TableError(..), SessionError(..))


instance Error SessionError where
  toType SessionDoesNotExist     = NotFound
  toReadable SessionDoesNotExist = "Session doesn't exist"


instance Error TableError where
  toType TableNotFound  = NotFound
  toType NameTaken      = Conflict
  toType PlayerNotFound = Forbidden

  toReadable TableNotFound  = "Table doesn't exist"
  toReadable NameTaken      = "Name is already taken"
  toReadable PlayerNotFound = "You're not a player on this table"
