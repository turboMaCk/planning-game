{-# LANGUAGE OverloadedStrings #-}

module PlanningGame.Api.Authorization.Type where

import PlanningGame.Api.Error (Error (..),
                               ErrorType (..))

data AuthorizationError
  = SessionNotFound
  | SessionIdMissing
  deriving (Show, Eq)

instance Error AuthorizationError where
  toType SessionNotFound  = Forbidden
  toType SessionIdMissing = Unauthorized

  toReadable SessionNotFound  = "Session expired."
  toReadable SessionIdMissing = "Session required."
