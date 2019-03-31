{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module PlanningGame.Data.Player
  ( Player(..)
  , Players
  , PlayerError(..)
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

import           Data.Aeson.Types        (ToJSON (..), object, (.=))
import           Data.IntMap.Strict      (IntMap)
import           Data.Map.Strict         (Map)
import           Data.Text               (Text)
import           Network.WebSockets      (Connection)

import qualified Data.IntMap             as IntMap
import qualified Data.Map                as Map
import qualified Data.Text               as Text
import qualified Network.WebSockets      as WS

import           PlanningGame.Api.Error.Class        (Error (..),
                                                      ErrorType (..))
import           PlanningGame.Data.Id      (Id)
import           PlanningGame.Data.Session


data Player = Player
  { playerName        :: Text
  , playerConnections :: IntMap Connection
  }


instance Show Player where
   show Player { playerName=name } = "Name: " <> show name


instance ToJSON Player where
  toJSON player@(Player { playerName=name }) =
    object
        [ "name" .= name
        , "connected" .= hasConnection player
        ]


type Players =
  Map (Id SessionId) Player


data PlayerError
  = NameTaken
  | NameEmpty
  deriving (Show, Eq)


instance Error PlayerError where
  toType NameTaken = Conflict
  toType NameEmpty = Unprocessable

  toReadable NameTaken = "Player with this name already exists."
  toReadable NameEmpty = "Name can't be empty."


createPlayer :: Text -> Player
createPlayer n =
  Player n IntMap.empty


nameTaken :: Text -> Players -> Bool
nameTaken name sessions =
  not $ Map.null $
    Map.filter ((==) name . playerName) sessions


addPlayer :: Session -> Text -> Players -> Either PlayerError ( Players, Player )
addPlayer id' name players =
  if Text.null name then
    Left NameEmpty
  else if nameTaken name players then
    Left NameTaken
  else
    let
      newPlayer =
        createPlayer name
    in
    Right ( Map.insert id' newPlayer players, newPlayer )


addConnectionToPlayer :: Connection -> Player -> (Player, Int)
addConnectionToPlayer conn player@Player { playerConnections } =
  (updatedPlayer, index)

  where
    index =
      IntMap.size playerConnections

    updatedPlayer =
      player { playerConnections = IntMap.insert index conn playerConnections }


hasConnection :: Player -> Bool
hasConnection Player { playerConnections } =
  not $ IntMap.null playerConnections


playerNumberOfConnections :: Player -> Int
playerNumberOfConnections Player { playerConnections } =
  IntMap.size playerConnections


addPlayerConnection :: Id SessionId -> Connection -> Players -> ( Players, Maybe ( Player, Int ) )
addPlayerConnection id' conn players =
  case Map.lookup id' players of
    Just player ->
      let
        ( updatedPlayer, index ) =
          addConnectionToPlayer conn player
      in
      ( Map.insert id' updatedPlayer players
      , Just ( updatedPlayer, index )
      )

    Nothing ->
      ( players, Nothing )


removeConnectionFromPlayer :: Int -> Player -> Player
removeConnectionFromPlayer index player@(Player { playerConnections }) =
      player { playerConnections = IntMap.delete index playerConnections }


disconnectPlayer :: Id SessionId -> Int -> Players -> Players
disconnectPlayer sessionId index =
  Map.alter (fmap $ removeConnectionFromPlayer index) sessionId


getPlayer :: Id SessionId -> Players -> Maybe Player
getPlayer = Map.lookup


emptyPlayers :: Players
emptyPlayers = Map.empty


allPlayerConnections :: Player -> [ WS.Connection ]
allPlayerConnections Player { playerConnections } =
  snd <$> IntMap.toList playerConnections
