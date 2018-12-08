{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AgilePoker.Data.Table.Msg where

import Control.Monad (mzero)
import qualified Data.Text as T
import qualified Data.Aeson.Types as AT
import Data.Aeson.Types (FromJSON(..), (.:))

import AgilePoker.Data.Game


data Msg
  = NewGame T.Text
  | NextRound Vote T.Text
  | Vote Vote
  | FinishRound
  | FinishGame Vote


instance FromJSON Msg where
  parseJSON obj@(AT.Object v) =
    (v .: "msg") >>= \(msg :: String) ->
       case msg of
         "NewGame" ->
           NewGame <$> (v .: "name")

         "NextRound" ->
           NextRound
             <$> (v .: "vote")
             <*> (v .: "name")

         "Vote" ->
           Vote <$> (v .: "vote")

         "FinishRound" ->
           pure FinishRound

         "FinishGame" ->
           FinishGame <$> (v .: "vote")

         _ ->
           mzero

  parseJSON _ = mzero
