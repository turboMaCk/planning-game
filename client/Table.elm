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
    , players : Dict String Player
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


updatePlayer : Player -> Model -> Model
updatePlayer player model =
    if Just player.name == Maybe.map .name model.banker then
        { model | banker = Just player }

    else
        { model | players = Dict.insert player.name player model.players }


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
            ( updatePlayer player model
            , Cmd.none
            )

        PlayerStatusUpdate player ->
            ( updatePlayer player model
            , Cmd.none
            )

        SyncTableState table ->
            ( { model
                | tableId = table.id
                , banker = Just table.banker
                , players =
                    List.filter ((/=) (Maybe.map .name model.me) << Just << .name) table.players
                        |> List.map (\player -> ( player.name, player ))
                        |> Dict.fromList
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Stream.observe Event


viewUser : Model -> Player -> Html msg
viewUser { me, banker } player =
    Html.li []
        [ Html.text player.name
        , if player.isConnected then
            Html.text " o"

          else
            Html.text " x"
        , if Maybe.map .name banker == Just player.name then
            Html.text " banker"

          else
            Html.text ""
        , if Maybe.map .name me == Just player.name then
            Html.text " change name"

          else
            Html.text ""
        ]


view : Model -> Html Msg
view model =
    Component.withTableNotFound model.tableError <|
        Html.div []
            [ Html.ul [] <|
                Maybe.unwrap (Html.text "") (viewUser model) model.banker
                    :: (List.map (viewUser model) <|
                            Dict.values model.players
                       )
            ]
