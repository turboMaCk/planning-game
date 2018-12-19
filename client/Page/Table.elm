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
import Page.Table.Card as Card exposing (Side(..))
import Page.Table.Players as Players
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


viewTable : (Vote -> Side) -> Html Msg
viewTable toSide =
    Html.styled Html.div [ Css.marginLeft <| Css.px -12 ] [] <|
        Card.table toSide Vote


tableContainer : String -> List (Html msg) -> Html msg
tableContainer text inner =
    Html.main_ [] <|
        Html.styled Html.p
            [ Css.fontSize <| Css.px 20 ]
            []
            [ Html.text text ]
            :: inner


viewVoting : Model -> Html Msg
viewVoting model =
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
    tableContainer "Pick your card:"
        [ viewTable toSide
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
    tableContainer "Select agreed estimation:"
        [ viewTable toSide
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
                    viewVoting model

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


currentGameView : Game -> Html msg
currentGameView game =
    let
        headlineStyle =
            Css.batch
                [ Css.margin2 (Css.px 12) Css.zero
                , Css.fontSize <| Css.px 27
                ]

        headline =
            Html.styled Html.h2

        showName name =
            headline [ headlineStyle ]
                []
                [ Html.styled Html.span
                    [ Css.fontWeight <| Css.int 200
                    , Css.display Css.block
                    , Css.fontSize <| Css.px 20
                    , Css.marginBottom <| Css.px 6
                    ]
                    []
                    [ Html.text "Estimating ticket:" ]
                , Html.text <|
                    if String.isEmpty name then
                        "[unnamed]"

                    else
                        name
                ]
    in
    case game of
        NotStarted ->
            headline [ Css.fontWeight <| Css.int 200 ]
                []
                [ Html.text "Waiting for first ticket" ]

        Voting { name } ->
            showName name

        RoundFinished { name } ->
            showName name

        Overview _ ->
            headline [ Css.fontWeight <| Css.int 200 ]
                []
                [ Html.text "Table overview" ]


pointsSoFarView : Game -> Html msg
pointsSoFarView game =
    let
        showPoints int =
            Html.styled Html.h3
                [ Css.margin Css.zero
                , Css.fontSize <| Css.px 37
                ]
                []
                [ Html.styled Html.span
                    [ Css.fontWeight <| Css.int 200
                    , Css.fontSize <| Css.px 20
                    , Css.display Css.block
                    ]
                    []
                    [ Html.text "Points so far:" ]
                , Html.text <| String.fromInt int
                ]
    in
    case game of
        NotStarted ->
            Html.text ""

        Voting { totalPoints } ->
            showPoints totalPoints

        RoundFinished { totalPoints } ->
            showPoints totalPoints

        Overview { totalPoints } ->
            showPoints totalPoints


view : Model -> Html Msg
view model =
    Component.withTableNotFound model.tableError <|
        Html.div []
            [ viewGame model
            , Html.styled Html.aside
                [ Css.float Css.left ]
                []
                [ currentGameView model.game
                , pointsSoFarView model.game
                , Players.view model.me model.banker model.players
                ]
            ]
