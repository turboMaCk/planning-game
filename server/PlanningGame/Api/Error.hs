{-# LANGUAGE OverloadedStrings #-}

module PlanningGame.Api.Error
  ( module PlanningGame.Api.Error.Class
  ) where

import           PlanningGame.Api.Error.Class        (Error (..),
                                                      ErrorType (..),
                                                      respondError)
