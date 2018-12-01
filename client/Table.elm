module Table exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra as Cmd
import Component
import Data exposing (ApiError, Player, Table, TableError(..))
import Dict exposing (Dict)
import Html exposing (Html)
import Http
import Maybe.Extra as Maybe
import Stream exposing (Event(..), StreamError)
import Url.Builder as Url


{-| @TODO: Think if tableError and me shouldn't be
single Result type
-}
type alias Model =
    { tableId : String
    , me : Maybe Player
    , banker : Maybe Player
    , players : Dict String Bool
    , tableError : Maybe (ApiError TableError)
    }


init : String -> String -> ( Model, Cmd Msg )
init token id =
    ( { tableId = id
      , me = Nothing
      , banker = Nothing
      , players = Dict.empty
      , tableError = Nothing
      }
    , Data.getMe token id Me
    )


type Msg
    = Me (Result (ApiError TableError) Player)
    | NoOp
    | Event (Result StreamError Event)


update : Key -> Msg -> Model -> ( Model, Cmd Msg )
update navigationKey msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Me result ->
            case result of
                Ok player ->
                    ( { model | me = Just player }
                    , Stream.connect model.tableId
                    )

                Err err ->
                    ( { model | tableError = Just err }
                    , if Data.errorIs PlayerNotFound err then
                        Navigation.pushUrl navigationKey <|
                            Url.absolute [ "table", model.tableId, "join" ] []

                      else
                        Cmd.none
                    )

        Event result ->
            case result of
                Ok e ->
                    handleEvent e model

                Err e ->
                    -- @TODO: handle errors
                    ( model, Cmd.none )


handleEvent : Event -> Model -> ( Model, Cmd msg )
handleEvent event model =
    case event of
        PlayerJoin player ->
            ( { model | players = Dict.insert player.name player.isConnected model.players }
            , Cmd.none
            )

        PlayerStatusUpdate player ->
            ( { model | players = Dict.insert player.name player.isConnected model.players }
            , Cmd.none
            )

        SyncTableState table ->
            ( { model
                | tableId = table.id
                , banker = Just table.banker
                , players =
                    Dict.fromList <|
                        -- @TODO: remove me?
                        List.map (\{ name, isConnected } -> ( name, isConnected )) table.players
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Stream.observe Event


view : Model -> Html Msg
view model =
    Component.withTableNotFound model.tableError <|
        Html.div []
            [ Html.text "you "
            , Html.text <| Maybe.unwrap "" .name model.me
            , Html.ul [] <|
                List.map
                    (\( n, online ) ->
                        Html.li []
                            [ Html.text <|
                                n
                                    ++ ": "
                                    ++ (if online then
                                            "o"

                                        else
                                            "x"
                                       )
                            ]
                    )
                <|
                    Dict.toList model.players
            ]
