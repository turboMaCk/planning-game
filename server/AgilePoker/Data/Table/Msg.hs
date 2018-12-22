{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AgilePoker.Data.Table.Msg (Msg(..)) where

import           Control.Monad        (mzero)
import           Data.Aeson.Types     (FromJSON (..), (.:))
import qualified Data.Aeson.Types     as AT
import qualified Data.Text            as T

import           AgilePoker.Data.Game


data Msg
  = NewGame T.Text
  | FinishRound
  | NextRound Vote T.Text
  | Vote Vote
  | FinishGame Vote


instance FromJSON Msg where
  parseJSON obj@(AT.Object v) =
    (v .: "msg") >>= \(msg :: String) ->
       case msg of
         "NewGame" ->
           NewGame <$> (v .: "name")

         "FinishRound" ->
           pure FinishRound

         "NextRound" ->
           NextRound
             <$> (v .: "vote")
             <*> (v .: "name")

         "Vote" ->
           Vote <$> (v .: "vote")

         "FinishGame" ->
           FinishGame <$> (v .: "vote")

         _ ->
           mzero

  parseJSON _ = mzero
