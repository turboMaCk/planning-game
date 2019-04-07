{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PlanningGame.Data.Table
  ( Table(..)
  , Tables
  , TableId
  , TableError(..)
  , empty
  , create
  , getPlayer
  , isActive
  , isBanker
  , lookup
  , allPlayers
  , assignConnection
  , allConnections
  ) where

import           Prelude                       hiding (lookup)
import           Control.Concurrent            (MVar)
import           Data.Aeson.Types              (ToJSON (..), (.=), Value (..))
import           Data.Map                      (Map)
import           Data.Text                     (Text)
import           Data.Time.Clock               (UTCTime)
import           Network.WebSockets            (Connection)

import qualified Control.Concurrent            as Concurrent
import qualified Data.Aeson                    as Aeson
import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as Maybe
import qualified Data.Text                     as Text
import qualified Data.Time.Clock               as Clock

import           PlanningGame.Api.Error        (Error (..), ErrorType (..))
import           PlanningGame.Api.GameSnapshot (snapshot)

import           PlanningGame.Data.Game        (GameError, Games)
import           PlanningGame.Data.Id          (Id, generateId)
import           PlanningGame.Data.Player      (Player, PlayerError (..),
                                                Players)
import           PlanningGame.Data.Session     (Session, SessionId)

import qualified PlanningGame.Data.Player      as Player


-- Types


data TableId


data Table = Table
  { tableId   :: Id TableId
  , banker    :: ( Id SessionId, Player )
  , players   :: Players
  , game      :: Maybe Games
  , createdAt :: UTCTime
  }


instance ToJSON Table where
  toJSON table =
    Aeson.object
        [ "id"      .= tableId table
        , "banker"  .= snd (banker table)
        , "players" .= fmap snd (Player.toList $ players table)
        , "game"    .=
          case game table of
            Just game ->
              toJSON $ snapshot (banker table) (players table) game

            Nothing ->
              Null
        ]


type Tables =
  Map (Id TableId) (MVar Table)


data TableError
  = TableNotFound
  | PlayerNotFound
  | PlayerError PlayerError
  | GameError GameError
  deriving (Eq)


instance Show TableError where
  show TableNotFound   = "TableNotFound"
  show PlayerNotFound  = "PlayerNotFound"
  show (PlayerError e) = "PlayerError:" <> show e
  show (GameError e)   = "GameError:" <> show e


instance Error TableError where
  toType TableNotFound   = NotFound
  toType PlayerNotFound  = Forbidden
  toType (GameError e)   = toType e
  toType (PlayerError e) = toType e

  toReadable TableNotFound   = "Table doesn't exist."
  toReadable PlayerNotFound  = "You're not a player on this table."
  toReadable (GameError e)   = toReadable e
  toReadable (PlayerError e) = toReadable e


empty :: Tables
empty =
  Map.empty


-- Basic Operations


create :: Session -> Text -> Tables -> IO ( Tables, Either TableError Table )
create id' name' tables =
  let
    name =
      Text.strip name'
  in
  if Text.null name then
    pure ( tables,  Left $ PlayerError NameEmpty )

  else do
    tId <- generateId tables
    now <- Clock.getCurrentTime

    let banker' = Player.create name
    let newTable = Table tId ( id', banker' ) Player.empty Nothing now

    mvarTable <- Concurrent.newMVar newTable

    pure
        ( Map.insert tId mvarTable tables
        , Right newTable
        )


isActive :: Table -> Bool
isActive Table { banker, players } =
  bankerOnline || Player.anyOnline players

  where
    bankerOnline =
      Player.hasConnection $ snd banker


getPlayer :: Session -> Id TableId -> Tables -> IO (Either TableError Player)
getPlayer session tableId tables =
  Maybe.fromMaybe (pure $ Left TableNotFound) $ getPlayer' <$>
    Map.lookup tableId tables

  where
    getPlayer' :: MVar Table -> IO (Either TableError Player)
    getPlayer' mvar = do
      table <- Concurrent.readMVar mvar

      if fst (banker table) == session then
          pure $ Right $ snd (banker table)

      else
          pure $ maybe (Left PlayerNotFound) Right $
            Player.lookup session $ players table


allConnections :: Table -> [ Connection ]
allConnections Table { banker, players } =
  concat $ (Player.allConnections $ snd banker)
         : (foldr (\p acc -> Player.allConnections p : acc) [] players)


assignConnection :: Session -> Connection -> Table -> ( Table, Maybe ( Player, Int ) )
assignConnection session conn table@Table { banker, players } =
    if fst banker == session then
        let ( updatedBanker, connId ) = Player.addConnectionTo conn $ snd banker
        in
        ( table { banker = ( session, updatedBanker ) }
        , Just ( updatedBanker, connId )
        )

    else
        let
          ( updatedPlayers, mPair ) =
            Player.addConnection session conn players
        in
        case mPair of
            Nothing   -> ( table, Nothing )
            Just pair -> ( table { players = updatedPlayers }
                           , Just pair
                           )

allPlayers :: Table -> Players
allPlayers table =
  Player.insert bankerId banker' $ players table

  where
    (bankerId, banker') = banker table


isBanker :: Session -> Table -> Bool
isBanker session Table { banker=pair } =
  session == fst pair


lookup :: Id TableId -> Tables -> Maybe (MVar Table)
lookup = Map.lookup
