{-# LANGUAGE OverloadedStrings     #-}
module AgilePocker.UserInfo (UserInfo(..)) where

import Data.Aeson (FromJSON(..), (.:))
import qualified Data.Text as T
import qualified Data.Aeson.Types as AT

import AgilePocker.Session


data UserInfo = UserInfo
  { userName :: T.Text
  }
  deriving (Eq, Show)

instance FromJSON UserInfo where
  parseJSON obj@(AT.Object v) = UserInfo <$> (v .: "name")
