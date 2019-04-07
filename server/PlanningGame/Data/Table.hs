{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PlanningGame.Data.Table
  ( Table(..)
  , Tables
  , TableId
  , Event
  , TableError(..)
  , empty
  , create
  , join
  , getPlayer
  , streamHandler
  , isActive
  ) where

import           Control.Concurrent            (MVar)
import           Control.Exception             (finally)
import           Control.Monad                 (forM_, forever, mzero)
import           Data.Aeson.Types              (ToJSON (..), Value (..), object,
                                                (.=))
import           Data.ByteString               (ByteString)
import           Data.Map                      (Map)
import           Data.Maybe                    (fromMaybe, isNothing)
import           Data.Text                     (Text)
import           Data.Time.Clock               (UTCTime)
import           Network.WebSockets            (Connection)

import qualified Control.Concurrent            as Concurrent
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy          as LazyByteString
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import qualified Data.Time.Clock               as Clock
import qualified Network.WebSockets            as WS

import           PlanningGame.Api.Error        (Error (..), ErrorType (..))
import           PlanningGame.Api.GameSnapshot (snapshot)

import           PlanningGame.Data.Game        (GameError, Games)
import           PlanningGame.Data.Id          (Id, generateId)
import           PlanningGame.Data.Player      (Player, PlayerError (..),
                                                Players)
import           PlanningGame.Data.Session     (Session, SessionId)

import           PlanningGame.Data.Table.Msg   (Msg (..))

import qualified PlanningGame.Data.Game        as Game
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
    object
        [ "id"      .= tableId table
        , "banker"  .= snd (banker table)
        , "players" .= fmap snd (Map.toList $ players table)
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


-- Event


data Event
    = PlayerJoined Player
    | PlayerStatusUpdate Player
    | SyncTableState Table
    | GameStarted ( Id SessionId, Player ) Players Games
    | VoteAccepted Player
    | VotingEnded ( Id SessionId, Player ) Players Games
    | GameEnded ( Id SessionId, Player ) Players Games


instance ToJSON Event where
  toJSON (PlayerJoined player) =
    object
        [ "event"  .= Text.pack "PlayerJoined"
        , "player" .= player
        ]
  toJSON (PlayerStatusUpdate player) =
    object
        [ "event"  .= Text.pack "PlayerStatusUpdate"
        , "player" .= player
        ]
  toJSON (SyncTableState table) =
    object
        [ "event"        .= Text.pack "SyncTableState"
        , "table"        .= table
        , "nextGameName" .= maybe "Task-1" Game.autoNextName (game table)
        ]
  toJSON (GameStarted dealer players games) =
    object
        [ "event" .= Text.pack "GameStarted"
        , "game"  .= snapshot dealer players games
        ]
  toJSON (VoteAccepted player) =
    object
        [ "event"  .= Text.pack "VoteAccepted"
        , "player" .= player
        ]
  toJSON (VotingEnded dealer players games) =
    object
        [ "event"        .= Text.pack "VotingEnded"
        , "game"         .= snapshot dealer players games
        , "nextGameName" .= Game.autoNextName games
        ]
  toJSON (GameEnded dealer players games) =
    object
        [ "event" .= Text.pack "GameEnded"
        , "game"  .= snapshot dealer players games
        ]


encodeEvent :: Event -> ByteString
encodeEvent =
  LazyByteString.toStrict . Aeson.encode


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
  bankerOnline || anyPlayerOnline

  where
    bankerOnline =
      Player.hasConnection $ snd banker

    anyPlayerOnline =
      not $ Map.null $ Map.filter Player.hasConnection players


-- @TODO: Add check if session is not already present
join :: Session -> Id TableId -> Text -> Tables -> IO ( Either TableError Table )
join session tableId name' tables =
  let
    name =
      Text.strip name'
  in
  case Map.lookup tableId tables of
    Just mvar -> do
      table <- Concurrent.readMVar mvar

      if Player.name (snd $ banker table) == name then
        pure $ Left $ PlayerError NameTaken

      else
        let
            ePlayers = Player.add session name (players table)
        in
        case ePlayers of
          Right ( newPlayers, newPlayer ) ->
            Concurrent.modifyMVar mvar $ \t -> do
                let updatedTable = t { players = newPlayers }

                -- Broadcast to connections
                broadcast t $ PlayerJoined newPlayer

                pure ( updatedTable, Right updatedTable )

          Left err ->
             pure $ Left $ PlayerError err

    Nothing ->
      pure $ Left TableNotFound


getPlayer :: Session -> Id TableId -> Tables -> IO (Either TableError Player)
getPlayer session tableId tables =
  fromMaybe (pure $ Left TableNotFound) $ getPlayer' <$>
    Map.lookup tableId tables

  where
    getPlayer' :: MVar Table -> IO (Either TableError Player)
    getPlayer' mvar = do
      table <- Concurrent.readMVar mvar

      if fst (banker table) == session then
          pure $ Right $ snd (banker table)

      else
          pure $ maybe (Left PlayerNotFound) Right $
            Map.lookup session $ players table


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


allTablePlayers :: Table -> Players
allTablePlayers table =
  Map.insert bankerId banker' $ players table

  where
    (bankerId, banker') = banker table


-- WS Handling


handleStreamMsg :: Session -> MVar Table -> Connection -> IO ()
handleStreamMsg session state conn = forever $ do
  bs :: LazyByteString.ByteString <- WS.receiveData conn
  let decoded :: Maybe Msg = Aeson.decode bs

  case decoded of
    -- @TODO: handle unrecosinable msg
    Nothing  -> pure ()
    Just msg ->
      Concurrent.modifyMVar_ state $ handleMsg conn session msg


disconnect :: MVar Table -> Id SessionId -> Int -> IO ()
disconnect state sessionId connId =
  Concurrent.modifyMVar_ state $ \table@Table { banker=banker' } ->
    if fst banker' == sessionId then do
      let updatedTable = table { banker = ( fst banker' , Player.removeConnectionFrom connId $ snd banker' ) }
      let player = snd $ banker updatedTable

      if Player.hasConnection player then
        pure ()

      else
        broadcast updatedTable $ PlayerStatusUpdate player

      pure updatedTable

    else do
      let updatedTable = table { players = Player.disconnect sessionId connId (players table) }
      let mPlayer = Map.lookup sessionId $ players updatedTable

      maybe mzero (broadcast updatedTable . PlayerStatusUpdate) mPlayer
      pure updatedTable


streamHandler :: MVar Tables -> Session -> Id TableId -> Connection -> IO ()
streamHandler state session id' conn = do
  tables <- Concurrent.readMVar state
  let mTable = Map.lookup id' tables

  case mTable of
    Just tableState -> do

        -- 1. Assign connection to player
        mConnId <- Concurrent.modifyMVar tableState $ pure . assignConnection session conn

        -- 2. Sync sate to new player
        table <- Concurrent.readMVar tableState
        WS.sendTextData conn $ encodeEvent $ SyncTableState table

        -- 3. Start player handler
        case mConnId of
            Just ( player, connId ) ->

                -- 3.1 Remove connection on disconnection
                flip finally (disconnect tableState session connId) $ do

                    -- 3.2 Ping Thread
                    WS.forkPingThread conn 30

                    -- 3.3 Broadcast join event
                    if Player.numberOfConnections player == 1 then do
                        table' <- Concurrent.readMVar tableState
                        broadcast table' $ PlayerStatusUpdate player

                    else
                        mzero

                    -- 3.4 Delegate to Msg handler
                    handleStreamMsg session tableState conn

            Nothing ->
                -- @TODO: player doesn't exist (session is not a member)
                mzero

    Nothing -> do
        -- @TODO: handle table doesn't exist
        putStrLn "Table not found"
        mzero


broadcast :: Table -> Event -> IO ()
broadcast table event = do
  forM_ (allConnections table) $ flip WS.sendTextData $ encodeEvent event


isBanker :: Session -> Table -> Bool
isBanker session Table { banker=pair } =
  session == fst pair


-- MSG and Event handling


-- @TODO: refactor
handleMsg :: Connection -> Session -> Msg -> Table -> IO Table
handleMsg _ session (NewGame name) table
  | isBanker session table
  , isNothing (game table) = do
      let game = Game.start name
      let players' = players table
      broadcast table $ GameStarted (banker table) players' game
      pure $ table { game = Just game }

  | not $ isBanker session table = do
      -- @TODO: Handle forbidden action
      pure $ table

  | otherwise =
      -- @TODO: Handle already started
      pure $ table

handleMsg _ session FinishRound table
  | isBanker session table =
      case game table of
        Just games -> do
          let newGames = Game.finishCurrent games
          broadcast table $ VotingEnded (banker table) (players table) newGames
          pure $ table { game = Just newGames }

        Nothing ->
          -- @TODO: handled non started game
          pure table

  | otherwise =
    -- @TODO: handle forbidden
    pure table

handleMsg _ session (NextRound vote name) table
  | isBanker session table =
      case game table of
        Just games -> do
          case Game.nextRound vote name games of
            Left _ ->
              -- @TODO: missing err handling
              pure table
            Right newGames -> do
              broadcast table $ GameStarted (banker table) (players table) newGames
              pure $ table { game = Just newGames }
        Nothing ->
          -- @TODO: handled non started game
          pure table

  | otherwise =
    -- @TODO: handle forbidden
    pure table

handleMsg _ session (Vote vote) table =
  case game table of
    Just game ->
      case Game.addVote session vote game of
        Right newGames -> do
          maybe (pure ()) (broadcast table . VoteAccepted) $ Player.get session $ allTablePlayers table

          -- Auto end game when all voted
          if Game.allVoted (allTablePlayers table) newGames then do
            let finishedNewGames = Game.finishCurrent newGames

            broadcast table $ VotingEnded (banker table) (players table) finishedNewGames
            pure $ table { game = Just finishedNewGames }

          else
            pure $ table { game = Just newGames }

        -- @TODO: can't vote error
        Left _ ->
          pure table

    Nothing ->
      -- @TODO: hanlde not started
      pure table

handleMsg _ session (FinishGame vote) table
  | isBanker session table =
    case game table of
      Just games -> do
        case Game.complete vote games of
          Right newGames -> do
            broadcast table $ GameEnded (banker table) (players table) newGames
            pure $ table { game = Just newGames }

          Left _ ->
            -- @TODO: handle already canceled
            pure table

      Nothing ->
        -- @TODO: handle game wasn't started
        pure table

  | otherwise =
      -- @TODO: handle forbidden
      pure table

handleMsg _ session RestartRound table
  | isBanker session table =
    case game table of
      Just g -> do
        let game = Game.restartCurrent g
        let players' = players table
        broadcast table $ GameStarted (banker table) players' game
        pure $ table { game = Just game }

      Nothing ->
        -- @TODO: handle err
        pure table

  | otherwise =
      -- @TODO: handle forbidden
      pure table

handleMsg _ session (KickPlayer name) table
  | isBanker session table = do
    let sesId = Player.getSessionId name $ players table
    case sesId of
      Just id' ->
        pure $ table
            { players = Player.kick name $ players table
            , game = Game.removePlayerVotes id' <$> game table
            }

      Nothing ->
        pure table

  | otherwise =
      -- @TODO: handle forbidden
      pure table
