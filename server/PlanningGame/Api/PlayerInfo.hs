{-# LANGUAGE OverloadedStrings #-}

module PlanningGame.Api.PlayerInfo (PlayerInfo (..)) where

import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), (.:))
import Data.Aeson.Types (Value (..))
import Data.Text (Text)
import PlanningGame.Data.Player (PlayerStatus)


data PlayerInfo = PlayerInfo
    { name :: !Text
    , status :: !PlayerStatus
    }
    deriving (Eq, Show)


instance FromJSON PlayerInfo where
    parseJSON (Object v) =
        PlayerInfo
            <$> (v .: "name")
            <*> (v .: "status")
    parseJSON _ = mzero
