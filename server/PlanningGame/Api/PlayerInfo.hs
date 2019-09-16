{-# LANGUAGE OverloadedStrings #-}

module PlanningGame.Api.PlayerInfo (PlayerInfo(..)) where

import           Control.Monad    (mzero)
import           Data.Aeson       (FromJSON (..), (.:))
import           Data.Aeson.Types (Value (..))
import           Data.Text        (Text)


data PlayerInfo = PlayerInfo
  { name     :: Text
  , isActive :: Bool
  }
  deriving (Eq, Show)

instance FromJSON PlayerInfo where
  parseJSON (Object v) = PlayerInfo <$> (v .: "name")
                            <*> (v .: "isActive")
  parseJSON _          = mzero
