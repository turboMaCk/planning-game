{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module AgilePoker.Data.Table
  (Table(..), Tables, TableId, Event, TableError(..)
  , emptyTables, createTable, joinTable
  , getTablePlayer
  , tableStreamHandler
  ) where

import Data.Maybe (fromMaybe, isNothing, isJust)
import Control.Monad (forM_, forever, mzero)
import Control.Concurrent (MVar)
import Control.Exception (finally)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Control.Concurrent as Concurrent
import qualified Data.Aeson as Aeson

import AgilePoker.Data.Id (Id, generateId)
import AgilePoker.Data.Session
import AgilePoker.Data.Player
import AgilePoker.Data.Game
import AgilePoker.Data.Table.Msg

import AgilePoker.Data.Table.Event
import AgilePoker.Data.Table.Type


-- Basic Operations


createTable :: Session -> T.Text -> Tables -> IO ( Tables, Table )
createTable Session { sessionId=id' } name tables = do
  tId <- generateId tables
  let banker = createPlayer name
  let newTable = Table tId ( id', banker ) emptyPlayers Nothing
  mvarTable <- Concurrent.newMVar newTable
  pure $ ( Map.insert tId mvarTable tables
         , newTable
         )


-- @TODO: Add check if session is not already present
joinTable :: Session -> Id TableId -> T.Text -> Tables -> IO ( Either TableError Table )
joinTable session@Session { sessionId=id' } tableId name tables =
  case Map.lookup tableId tables of
    Just mvar -> do
      table <- Concurrent.readMVar mvar
      if playerName (snd $ tableBanker table) == name then
        pure $ Left NameTaken
      else
        let
            mPlayers = addPlayer session name (tablePlayers table)
        in
        case mPlayers of
          Just (newPlayers, newPlayer) ->
            Concurrent.modifyMVar mvar $ \t -> do
                let updatedTable = t { tablePlayers = newPlayers }

                -- Broadcast to connections
                broadcast t $ PlayerJoined newPlayer

                pure ( updatedTable, Right updatedTable )
          Nothing ->
             pure $ Left NameTaken

    Nothing ->
      pure $ Left TableNotFound

  where
    updateTables :: Table -> Players -> IO ( Tables, Maybe Table )
    updateTables table players =
       undefined


getTablePlayer :: Session -> Id TableId -> Tables -> IO (Either TableError Player)
getTablePlayer Session { sessionId=sId } tableId tables =
  fromMaybe (pure $ Left TableNotFound) $ getPlayer' <$>
    Map.lookup tableId tables

  where
    getPlayer' :: MVar Table -> IO (Either TableError Player)
    getPlayer' mvar = do
      table <- Concurrent.readMVar mvar

      if fst (tableBanker table) == sId then
          pure $ Right $ snd (tableBanker table)
      else
          pure $ maybe (Left PlayerNotFound) Right $
            Map.lookup sId $ tablePlayers table


allConnections :: Table -> [ WS.Connection ]
allConnections Table { tableBanker=banker, tablePlayers=players } =
  concat $ (allPlayerConnections $ snd banker)
         : (foldr (\p acc -> allPlayerConnections p : acc) [] players)


assignConnection :: Id SessionId -> WS.Connection -> Table -> ( Table, Maybe ( Player, Int ) )
assignConnection sId conn table@Table { tableBanker=banker, tablePlayers=players } =
    if fst banker == sId then
        let ( updatedBanker, connId ) = addConnectionToPlayer conn $ snd banker
        in
        ( table { tableBanker = ( sId, updatedBanker ) }
        , Just ( updatedBanker, connId )
        )
    else
        let ( updatedPlayers, mPair ) = addPlayerConnection sId conn players
        in
        case mPair of
            Nothing     -> ( table, Nothing )
            Just pair -> ( table { tablePlayers = updatedPlayers }
                           , Just pair
                           )

-- WS Handling


handleStreamMsg :: Session -> MVar Table -> WS.Connection -> IO ()
handleStreamMsg session state conn = forever $ do
  bs :: ByteString <- WS.receiveData conn
  let decoded :: Maybe Msg = Aeson.decode bs
  case decoded of
    -- @TODO: handle unrecosinable msg
    Nothing  -> pure ()
    Just msg ->
      Concurrent.modifyMVar_ state $ handleMsg conn session msg


disconnect :: MVar Table -> Id SessionId -> Int -> IO ()
disconnect state sessionId connId =
  Concurrent.modifyMVar_ state $ \table@Table { tableBanker=banker, tablePlayers=players } ->
    if fst banker == sessionId then do
      let updatedTable = table { tableBanker = ( fst banker , removeConnectionFromPlayer connId $ snd banker ) }
      let player = snd $ tableBanker updatedTable

      if hasConnection player then
        pure ()
      else
        broadcast updatedTable $ PlayerStatusUpdate player

      pure updatedTable
    else do
      let updatedTable = table { tablePlayers = disconnectPlayer sessionId connId (tablePlayers table) }
      let mPlayer = Map.lookup sessionId $ tablePlayers updatedTable

      maybe mzero (broadcast updatedTable . PlayerStatusUpdate) mPlayer
      pure updatedTable


tableStreamHandler :: MVar Tables -> Session -> Id TableId -> WS.Connection -> IO ()
tableStreamHandler state session@Session { sessionId=sId } id' conn = do
  tables <- Concurrent.readMVar state
  let mTable = Map.lookup id' tables

  case mTable of
    Just tableState -> do

        -- 1. Assign connection to player
        mConnId <- Concurrent.modifyMVar tableState $ pure . assignConnection sId conn

        -- 2. Sync sate to new player
        table <- Concurrent.readMVar tableState
        WS.sendTextData conn $ encodeEvent $ SyncTableState table

        -- 3. Start player handler
        case mConnId of
            Just ( player, connId ) ->

                -- 3.1 Remove connection on disconnection
                flip finally (disconnect tableState sId connId) $ do

                    -- 3.2 Ping Thread
                    WS.forkPingThread conn 30

                    -- 3.3 Broadcast join event
                    if playerNumberOfConnections player == 1 then do
                        table <- Concurrent.readMVar tableState
                        broadcast table $ PlayerStatusUpdate player
                    else
                        mzero

                    -- 3.4 Delegate to Msg handler
                    handleStreamMsg session tableState conn

            Nothing -> do
                -- @TODO: player doesn't exist (session is not a member)
                putStrLn "User not a member"
                mzero

    Nothing -> do
        -- @TODO: handle table doesn't exist
        putStrLn "Table not found"
        mzero


broadcast :: Table -> Event -> IO ()
broadcast table event = do
  forM_ (allConnections table) $ flip WS.sendTextData $ encodeEvent event


isBanker :: Session -> Table -> Bool
isBanker Session { sessionId=id } Table { tableBanker=pair } =
  id == fst pair


-- MSG and Event handling


-- @TODO: refactor
handleMsg :: WS.Connection -> Session -> Msg -> Table -> IO Table
handleMsg conn session (NewGame name) table
  | isBanker session table
  , isNothing (tableGame table) = do
      let games = startGame name
      let players = tablePlayers table
      broadcast table $ GameStarted players games
      pure $ table { tableGame = Just games }
  | not $ isBanker session table = do
      -- @TODO: Handle forbidden action
      pure $ table
  | isJust (tableGame table) =
      -- @TODO: Handle already started
      pure $ table
handleMsg conn session FinishRound table
  | isBanker session table =
      case tableGame table of
        Just games -> do
          broadcast table $ VotingEnded (tablePlayers table) games
          pure $ table { tableGame = Just $ finishCurrentGame games }
        Nothing ->
          -- @TODO: handled non started game
          pure table
  | not $ isBanker session table =
    -- @TODO: handle forbidden
    pure table
handleMsg conn session (NextRound vote name) table
  | isBanker session table =
      case tableGame table of
        Just games -> do
          case nextRound vote name games of
            Left err ->
              -- @TODO: missing err handling
              pure table
            Right newGames -> do
              broadcast table $ GameStarted (tablePlayers table) newGames
              pure $ table { tableGame = Just newGames }
        Nothing ->
          -- @TODO: handled non started game
          pure table
  | not $ isBanker session table =
    -- @TODO: handle forbidden
    pure table
handleMsg conn session (Vote vote) table =
  case tableGame table of
    Just game ->
      let sId = (sessionId session) in
      case addVote sId vote game of
        Right newGames -> do
          maybe (pure ()) (broadcast table . VoteAccepted) $ getPlayer sId $ tablePlayers table
          pure $ table { tableGame = Just newGames }

        -- @TODO: can't vote error
        Left _ -> pure table

    Nothing ->
      -- @TODO: hanlde not started
      pure table
handleMsg conn session (FinishGame vote) table
  | isBanker session table =
    case tableGame table of
      Just games -> do
        case completeGame vote games of
          Right newGames ->
            broadcast table $ GameEnded
            pure $ table { tableGame = Just newGames }
          Left _ ->
            -- @TODO: handle already canceled
            pure table
      Nothing ->
        -- @TODO: handle game wasn't started
        pure table
  | not isBanker session table =
    -- @TODO: handle forbidden
    pure table
