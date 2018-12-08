{-# LANGUAGE OverloadedStrings     #-}

module AgilePoker.Api.UserInfo (UserInfo(..)) where

import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), (.:))
import qualified Data.Text as T
import qualified Data.Aeson.Types as AT


data UserInfo = UserInfo
  { userName :: T.Text
  }
  deriving (Eq, Show)

instance FromJSON UserInfo where
  parseJSON (AT.Object v) = UserInfo <$> (v .: "name")
  parseJSON _             = mzero
