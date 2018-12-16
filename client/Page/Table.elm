module Page.Table exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra as Cmd
import Component
import Data exposing (ApiError, Game(..), Player, Table, TableError(..), Vote(..))
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Html.Styled exposing (toUnstyled)
import Http
import Maybe.Extra as Maybe
import Page.Table.Card as Card exposing (Side(..))
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
    , myVote : Maybe Vote
    , game : Game
    , gameName : String
    }


init : String -> String -> ( Model, Cmd Msg )
init token id =
    ( { tableId = id
      , me = Nothing
      , banker = Nothing
      , players = Dict.empty
      , tableError = Nothing
      , myVote = Nothing
      , game = NotStarted
      , gameName = ""
      }
    , Data.getMe token id Me
    )


type Msg
    = Me (Result (ApiError TableError) Player)
    | NoOp
    | Event (Result StreamError Event)
    | Send Stream.Msg
    | Vote Vote


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

        Vote vote ->
            let
                nextMsg =
                    if Data.isVoting model.game then
                        Send <| Stream.Vote vote

                    else if Data.isRoundFinished model.game && amIBanker model then
                        Send <| Stream.NextGame model.gameName vote

                    else
                        NoOp
            in
            ( { model | myVote = Just vote }
            , Cmd.perform nextMsg
            )


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
            ( { model | game = game, myVote = Nothing }, Cmd.none )

        VoteAccepted player ->
            ( playerVoted player model, Cmd.none )

        VotingEnded game ->
            ( { model | game = game, myVote = Nothing }, Cmd.none )

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


votingView : Model -> Html Msg
votingView model =
    let
        toSide vote =
            case model.myVote of
                Just myVote ->
                    if vote == myVote then
                        Front

                    else
                        Back

                Nothing ->
                    Front
    in
    Html.main_
        [ Attrs.style "width" "835px"
        , Attrs.style "margin" "0 auto"
        ]
        [ Html.p
            [ Attrs.style "margin-left" "6px"
            , Attrs.style "font-weight" "200"
            ]
            [ Html.text "Pick your card:" ]
        , Html.div [] <| List.map toUnstyled <| Card.table toSide Vote
        , Html.br [] []
        , if amIBanker model then
            Html.button [ Events.onClick <| Send Stream.FinishRound ]
                [ Html.text "finish round" ]

          else
            Html.text ""
        ]


viewUserVotes : Dict String Vote -> Html Msg
viewUserVotes dict =
    let
        voteView ( name, vote ) =
            Html.tr []
                [ Html.td [] [ Html.text name ]
                , Html.td [] [ toUnstyled <| Card.view (always Front) Vote vote ]
                ]
    in
    Html.table [] <|
        List.map voteView <|
            Dict.toList dict


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
            votingView model

        RoundFinished { userVotes } ->
            Html.div []
                [ viewUserVotes userVotes
                , if amIBanker model then
                    Html.button [ Events.onClick <| Send <| Stream.NewGame "Test" ]
                        [ Html.text "Finish all voting" ]

                  else
                    Html.text ""
                ]

        Overview _ ->
            Html.text "overview"


playersView : Model -> Html Msg
playersView model =
    Html.ul [ Attrs.style "float" "left" ] <|
        Maybe.unwrap (Html.text "") (viewUser model) model.banker
            :: (List.map (viewUser model) <|
                    Dict.values model.players
               )


currentGameName : Game -> String
currentGameName game =
    case game of
        Voting { name } ->
            name

        RoundFinished { name } ->
            name

        _ ->
            ""


view : Model -> Html Msg
view model =
    Component.withTableNotFound model.tableError <|
        Html.div []
            [ Html.h2 [] [ Html.text <| currentGameName model.game ]
            , playersView model
            , viewGame model
            ]
