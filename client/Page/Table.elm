module Page.Table exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Dom as Dom
import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra as Cmd
import Component
import Css
import Data exposing (ApiError, Game(..), Player, Table, TableError(..), Vote(..))
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Http
import Maybe.Extra as Maybe
import Page.Table.Card as Card exposing (Side(..))
import Page.Table.Stream as Stream exposing (Event(..), StreamError)
import Set exposing (Set)
import Set.Any as AnySet
import Task
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
    , gameName : Maybe String
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
      , gameName = Just ""
      }
    , Data.getMe token id Me
    )


type Msg
    = Me (Result (ApiError TableError) Player)
    | NoOp
    | Event (Result StreamError Event)
    | Send Stream.Msg
    | Vote Vote
    | SetName String
    | NewGame String


focusNameField : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
focusNameField ( model, cmd ) =
    -- @TODO: add check on game state?
    if amIBanker model then
        ( model, cmd )
            |> Cmd.add (Task.attempt (always NoOp) <| Dom.focus nameFieldId)

    else
        ( model, cmd )


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
            if Data.isVoting model.game then
                { model | myVote = Just vote }
                    |> Cmd.with (Stream.sendMsg <| Stream.Vote vote)

            else if Data.isRoundFinished model.game && amIBanker model then
                ( { model | gameName = Just "", myVote = Just vote }, Cmd.none )
                    |> focusNameField

            else
                ( model, Cmd.none )

        SetName str ->
            ( { model | gameName = Just str }, Cmd.none )

        NewGame str ->
            case model.game of
                NotStarted ->
                    { model | gameName = Nothing }
                        |> Cmd.with (Stream.sendMsg <| Stream.NewGame str)

                RoundFinished _ ->
                    case model.myVote of
                        Just vote ->
                            { model | gameName = Nothing }
                                |> Cmd.with (Stream.sendMsg <| Stream.NextGame str vote)

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- NoOp


handleEvent : Event -> Model -> ( Model, Cmd Msg )
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
                |> focusNameField

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


viewTable : (Vote -> Side) -> Html Msg
viewTable toSide =
    Html.styled Html.div [ Css.marginLeft <| Css.px -6 ] [] <|
        Card.table toSide Vote


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
    Html.main_ []
        [ Html.p
            []
            [ Html.text "Pick your card:" ]
        , viewTable toSide
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
        voteSet =
            Dict.values dict
                |> AnySet.fromList Data.voteToInt

        toSide vote =
            if AnySet.member vote voteSet then
                Front

            else
                Back
    in
    Html.main_ []
        [ Html.p []
            [ Html.text "Grrr" ]
        , viewTable toSide
        ]


nameFieldId : String
nameFieldId =
    "game-name-field"


setNameView : Model -> Html Msg
setNameView model =
    let
        name =
            Maybe.withDefault "" model.gameName
    in
    if amIBanker model then
        Component.nameForm
            { onInput = SetName
            , onSubmit = NewGame name
            , submitTxt = "Submit"
            , value = name
            , inputId = nameFieldId
            , labelTxt = "Name of next task"
            }

    else
        -- @TODO: nice view
        Html.text "not a banker"


viewGame : Model -> Html Msg
viewGame model =
    let
        inner =
            case model.game of
                NotStarted ->
                    setNameView model

                Voting _ ->
                    votingView model

                RoundFinished { userVotes } ->
                    if amIBanker model && model.gameName /= Nothing then
                        setNameView model

                    else
                        viewUserVotes userVotes

                Overview _ ->
                    Html.text "overview"
    in
    Html.styled Html.div
        [ Css.width <| Css.px 835
        , Css.float Css.left
        ]
        []
        [ inner ]


viewPlayers : Model -> Html Msg
viewPlayers model =
    Html.ul [] <|
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
            [ viewGame model
            , Html.styled Html.aside
                [ Css.float Css.left ]
                []
                [ Html.h2 [] [ Html.text <| currentGameName model.game ]
                , viewPlayers model
                ]
            ]
