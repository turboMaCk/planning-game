{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Api.UserInfo (UserInfo(..)) where

import           Control.Monad    (mzero)
import           Data.Aeson       (FromJSON (..), (.:))
import           Data.Aeson.Types (Value (..))
import           Data.Text        (Text)


data UserInfo = UserInfo
  { userName :: Text
  }
  deriving (Eq, Show)

instance FromJSON UserInfo where
  parseJSON (Object v) = UserInfo <$> (v .: "name")
  parseJSON _          = mzero
