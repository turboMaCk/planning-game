{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Data.Player
  ( Player(..), Players, createPlayer
  , addPlayer, addPlayerConnection, disconnectPlayer
  , getPlayer, emptyPlayers, allPlayerConnections
  , addConnectionToPlayer, removeConnectionFromPlayer
  , hasConnection, playerNumberOfConnections
  ) where

import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Aeson.Types (ToJSON(..), (.=))
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Aeson.Types as AT

import AgilePoker.Data.Id
import AgilePoker.Data.Session


data Player = Player
  { playerName :: T.Text
  , playerConnections :: IntMap WS.Connection
  }


instance ToJSON Player where
  toJSON player@(Player { playerName=name }) =
    AT.object
        [ "name" .= name
        , "connected" .= hasConnection player
        ]


type Players = Map SessionId Player


createPlayer :: T.Text -> Player
createPlayer n = Player n IntMap.empty


nameTaken :: T.Text -> Players -> Bool
nameTaken name sessions = not $ Map.null $
  Map.filter ((==) name . playerName) sessions


addPlayer :: Session -> T.Text -> Players -> Maybe ( Players, Player )
addPlayer Session { sessionId=id' } name players =
  if nameTaken name players then
    Nothing
  else
    let newPlayer = createPlayer name
    in
    Just $ ( Map.insert id' newPlayer players, newPlayer )


addConnectionToPlayer :: WS.Connection -> Player -> (Player, Int)
addConnectionToPlayer conn player@Player { playerConnections=conns } =
  (updatedPlayer, index)
  where
    index = IntMap.size conns
    updatedPlayer = player { playerConnections = IntMap.insert index conn conns }


hasConnection :: Player -> Bool
hasConnection Player { playerConnections=conns } =
  not $ IntMap.null conns


playerNumberOfConnections :: Player -> Int
playerNumberOfConnections Player { playerConnections=conns } =
  IntMap.size conns


addPlayerConnection :: SessionId -> WS.Connection -> Players -> ( Players, Maybe ( Player, Int ) )
addPlayerConnection id' conn players =
  case Map.lookup id' players of
    Just player ->
      let ( updatedPlayer, index ) = addConnectionToPlayer conn player
      in
      ( Map.insert id' updatedPlayer players
      , Just ( updatedPlayer, index )
      )
    Nothing ->
      ( players, Nothing )


removeConnectionFromPlayer :: Int -> Player -> Player
removeConnectionFromPlayer index player@(Player { playerConnections=conns }) =
      player { playerConnections = IntMap.delete index conns }


disconnectPlayer :: SessionId -> Int -> Players -> Players
disconnectPlayer sessionId index =
  Map.alter (fmap $ removeConnectionFromPlayer index) sessionId


getPlayer :: SessionId -> Players -> Maybe Player
getPlayer = Map.lookup


emptyPlayers :: Players
emptyPlayers = Map.empty


allPlayerConnections :: Player -> [ WS.Connection ]
allPlayerConnections Player { playerConnections=conns } =
  snd <$> IntMap.toList conns
