{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Data.Player
  ( Player(..)
  , Players
  , createPlayer
  , addPlayer
  , addPlayerConnection
  , disconnectPlayer
  , getPlayer
  , emptyPlayers
  , allPlayerConnections
  , addConnectionToPlayer
  , removeConnectionFromPlayer
  , hasConnection
  , playerNumberOfConnections
  ) where

import           Data.Aeson.Types        (ToJSON (..), (.=))
import qualified Data.Aeson.Types        as AT
import qualified Data.IntMap             as IntMap
import           Data.IntMap.Strict      (IntMap)
import qualified Data.Map                as Map
import           Data.Map.Strict         (Map)
import qualified Data.Text               as T
import qualified Network.WebSockets      as WS

import           AgilePoker.Data.Id      (Id)
import           AgilePoker.Data.Session


data Player = Player
  { playerName        :: T.Text
  , playerConnections :: IntMap WS.Connection
  }


instance Show Player where
   show Player { playerName=name } = "Name: " <> show name


instance ToJSON Player where
  toJSON player@(Player { playerName=name }) =
    AT.object
        [ "name" .= name
        , "connected" .= hasConnection player
        ]


type Players = Map (Id SessionId) Player


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


addPlayerConnection :: Id SessionId -> WS.Connection -> Players -> ( Players, Maybe ( Player, Int ) )
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


disconnectPlayer :: Id SessionId -> Int -> Players -> Players
disconnectPlayer sessionId index =
  Map.alter (fmap $ removeConnectionFromPlayer index) sessionId


getPlayer :: Id SessionId -> Players -> Maybe Player
getPlayer = Map.lookup


emptyPlayers :: Players
emptyPlayers = Map.empty


allPlayerConnections :: Player -> [ WS.Connection ]
allPlayerConnections Player { playerConnections=conns } =
  snd <$> IntMap.toList conns
