module Page.Table exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra as Cmd
import Component
import Data exposing (ApiError, Game(..), Player, Table, TableError(..), Vote(..))
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Events as Events
import Http
import Maybe.Extra as Maybe
import Page.Table.Stream as Stream exposing (Event(..), StreamError)
import Set exposing (Set)
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
    , game : Game
    }


init : String -> String -> ( Model, Cmd Msg )
init token id =
    ( { tableId = id
      , me = Nothing
      , banker = Nothing
      , players = Dict.empty
      , tableError = Nothing
      , game = NotStarted
      }
    , Data.getMe token id Me
    )


type Msg
    = Me (Result (ApiError TableError) Player)
    | NoOp
    | Event (Result StreamError Event)
    | Send Stream.Msg


playerVoted : Player -> Model -> Model
playerVoted player model =
    case model.game of
        Voting data ->
            { model | game = Voting { data | maskedVotes = Set.insert player.name data.maskedVotes } }

        _ ->
            model


updatePlayer : Player -> Model -> Model
updatePlayer player model =
    if Just player.name == Maybe.map .name model.banker then
        { model | banker = Just player }

    else
        { model | players = Dict.insert player.name player model.players }


amIBanker : Model -> Bool
amIBanker { me, banker } =
    case ( me, banker ) of
        ( Just p1, Just p2 ) ->
            p1.name == p2.name

        _ ->
            False


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

        Send streamMsg ->
            ( model, Stream.sendMsg streamMsg )


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

        SyncTableState ( table, game ) ->
            ( { model
                | tableId = table.id
                , banker = Just table.banker
                , players =
                    List.filter ((/=) (Maybe.map .name model.me) << Just << .name) table.players
                        |> List.map (\player -> ( player.name, player ))
                        |> Dict.fromList
                , game = game
              }
            , Cmd.none
            )

        GameStarted game ->
            ( { model | game = game }, Cmd.none )

        VoteAccepted player ->
            ( playerVoted player model, Cmd.none )

        VotingEnded game ->
            ( { model | game = game }, Cmd.none )

        GameEnded game ->
            ( { model | game = game }, Cmd.none )


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


viewCard : Vote -> Html Msg
viewCard vote =
    let
        text =
            case vote of
                OnePoint ->
                    "1"

                TwoPoints ->
                    "2"

                ThreePoints ->
                    "3"

                FivePoints ->
                    "5"

                EightPoints ->
                    "8"

                ThreeteenPoints ->
                    "13"

                TwentyPoints ->
                    "20"

                FortyPoints ->
                    "40"

                HundredPoints ->
                    "100"

                InfinityPoints ->
                    "Infinity"

                UnknownPoints ->
                    "?"
    in
    Html.button [ Events.onClick <| Send <| Stream.Vote vote ] [ Html.text text ]


votingView : Html Msg
votingView =
    Html.main_ []
        [ viewCard OnePoint
        , viewCard TwoPoints
        , viewCard ThreePoints
        , viewCard FivePoints
        , viewCard EightPoints
        , viewCard ThreeteenPoints
        , viewCard TwentyPoints
        , viewCard FortyPoints
        , viewCard HundredPoints
        , viewCard InfinityPoints
        , viewCard UnknownPoints
        ]


viewGame : Model -> Html Msg
viewGame model =
    case model.game of
        NotStarted ->
            if amIBanker model then
                Html.button [ Events.onClick <| Send <| Stream.NewGame "Test" ]
                    [ Html.text "set name to test" ]

            else
                Html.text "not a banker"

        Voting _ ->
            votingView

        RoundFinished _ ->
            Html.text "finishing"

        Overview _ ->
            Html.text "overview"


view : Model -> Html Msg
view model =
    Component.withTableNotFound model.tableError <|
        Html.div []
            [ Html.ul [] <|
                Maybe.unwrap (Html.text "") (viewUser model) model.banker
                    :: (List.map (viewUser model) <|
                            Dict.values model.players
                       )
            , viewGame model
            ]
