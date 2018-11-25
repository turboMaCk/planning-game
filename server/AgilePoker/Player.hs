{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Player
  ( Player(..), Players, createPlayer
  , addPlayer, addPlayerConnection, disconnectPlayer
  , getPlayer, emptyPlayers
  ) where

import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Aeson.Types (ToJSON(..), (.=))
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Aeson.Types as AT

import AgilePoker.Id
import AgilePoker.Session


data Player = Player
  { playerName :: T.Text
  , playerConnections :: IntMap WS.Connection
  }


instance ToJSON Player where
  toJSON (Player { playerName=name, playerConnections=conns }) =
    AT.object
        [ "name" .= name
        , "connected" .= not (IntMap.null conns)
        ]


type Players = Map SessionId Player


createPlayer :: T.Text -> Player
createPlayer n = Player n IntMap.empty


nameTaken :: T.Text -> Players -> Bool
nameTaken name sessions = not $ Map.null $
  Map.filter ((==) name . playerName) sessions


addPlayer :: Session -> T.Text -> Players -> ( Players, Maybe Player )
addPlayer Session { sessionId=id' } name players =
  if nameTaken name players then
    ( players, Nothing )
  else
    let newPlayer = createPlayer name
    in
    ( Map.insert id' newPlayer players
    , Just newPlayer
    )


addPlayerConnection :: SessionId -> WS.Connection -> Players -> ( Players, Maybe ( Int, Player ) )
addPlayerConnection id' conn players =
  case Map.lookup id' players of
    Just (player@(Player { playerConnections=conns })) ->
      let index = IntMap.size conns
          updatedSession = (player { playerConnections = IntMap.insert index conn conns })
      in
      ( Map.insert id' updatedSession players
      , Just ( index, updatedSession )
      )
    Nothing ->
      ( players, Nothing )


disconnectPlayer :: ( SessionId, Int ) -> Players -> Players
disconnectPlayer ( sessionId, index ) = Map.alter (fmap update) sessionId
  where
    update :: Player -> Player
    update player@(Player { playerConnections=conns }) =
      player { playerConnections = IntMap.delete index conns }


getPlayer :: SessionId -> Players -> Maybe Player
getPlayer = Map.lookup


emptyPlayers :: Players
emptyPlayers = Map.empty
