module Page.Table exposing (Model, Msg, init, leave, subscriptions, update, view)

import Basics.Extra exposing (flip)
import Browser.Dom as Dom
import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra as Cmd
import Component
import Css
import Data exposing (ApiError, Game(..), Player, PlayerStatus(..), TableError(..), Vote(..))
import Dict exposing (Dict)
import Dict.Any as AnyDict
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Maybe.Extra as Maybe
import Page.Table.Card as Card
import Page.Table.Players as Players exposing (PlayerVote(..))
import Page.Table.Stream as Stream exposing (Event(..), StreamError)
import Set
import Task
import Theme
import Url.Builder as Url


{-| @TODO: Think if tableError and me shouldn't be
single Result type
-}
type alias Model =
    { tableId : String
    , me : Maybe Int
    , dealer : Maybe Int
    , players : Dict Int Player
    , tableError : Maybe (ApiError TableError)
    , myVote : Maybe Vote
    , game : Game
    , gameName : Maybe String
    , nextGameName : String
    , newName : Maybe String
    , focusedPlayer : Maybe Int
    , highlightedVote : Maybe Vote
    , newCurrentGameName : Maybe String
    , leaveConfirmation : Bool
    }


init : String -> String -> ( Model, Cmd Msg )
init token id =
    ( { tableId = id
      , me = Nothing
      , dealer = Nothing
      , players = Dict.empty
      , tableError = Nothing
      , myVote = Nothing
      , game = NotStarted
      , gameName = Just ""
      , nextGameName = "Task-1"
      , newName = Nothing
      , focusedPlayer = Nothing
      , highlightedVote = Nothing
      , newCurrentGameName = Nothing
      , leaveConfirmation = False
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
    | FinishGame
    | ClearMyVote
    | EditName
    | SetNewName String
    | SaveNewName
    | DiscardNewName
    | FocusPlayer (Maybe Int)
    | HighlightVote (Maybe Vote)
    | EditCurrentGameName
    | SetNewCurrentGameName String
    | SaveNewCurrentGameName
    | DiscardNewCurrentGameName
    | ToggleLeave


leave : () -> Cmd msg
leave () =
    Stream.disconnect ()


focusNameField : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
focusNameField ( model, cmd ) =
    -- @TODO: add check on game state?
    if amIDealer model then
        ( model, cmd )
            |> Cmd.add (Task.attempt (always NoOp) <| Dom.focus nameFieldId)

    else
        ( model, cmd )


playerVoted : Player -> Model -> Model
playerVoted player model =
    case model.game of
        Voting data ->
            { model
                | game =
                    Voting
                        { data
                            | maskedVotes =
                                Set.insert player.id data.maskedVotes
                        }
            }

        _ ->
            model


updatePlayer : Player -> Model -> Model
updatePlayer player model =
    { model | players = Dict.insert player.id player model.players }


amIDealer : Model -> Bool
amIDealer { me, dealer } =
    Just (==)
        |> Maybe.andMap me
        |> Maybe.andMap dealer
        |> Maybe.withDefault False


update : Key -> Msg -> Model -> ( Model, Cmd Msg )
update navigationKey msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Me result ->
            case result of
                Ok player ->
                    ( { model | me = Just player.id }
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
                    handleEvent navigationKey e model

                Err _ ->
                    -- @TODO: handle errors
                    -- Translate StreamError to ApiError TableError?
                    ( model, Cmd.none )

        Send streamMsg ->
            ( model, Stream.sendMsg streamMsg )

        Vote vote ->
            if Data.isVoting model.game then
                { model | myVote = Just vote }
                    |> Cmd.with (Stream.sendMsg <| Stream.Vote vote)

            else if Data.isRoundFinished model.game && amIDealer model then
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

        FinishGame ->
            case model.myVote of
                Just vote ->
                    { model | gameName = Nothing }
                        |> Cmd.with (Stream.sendMsg <| Stream.Finish vote)

                Nothing ->
                    ( model, Cmd.none )

        ClearMyVote ->
            ( { model | myVote = Nothing }, Cmd.none )

        EditName ->
            { model | newName = Just "" }
                |> Cmd.with (Task.attempt (always NoOp) <| Dom.focus editNameFieldId)

        SetNewName name ->
            ( { model | newName = Just name }, Cmd.none )

        SaveNewName ->
            { model | newName = Nothing }
                |> Cmd.with (Maybe.unwrap Cmd.none (Stream.sendMsg << Stream.ChangeName) model.newName)

        DiscardNewName ->
            ( { model | newName = Nothing }, Cmd.none )

        FocusPlayer state ->
            ( { model | focusedPlayer = state }, Cmd.none )

        HighlightVote state ->
            ( { model | highlightedVote = state }, Cmd.none )

        EditCurrentGameName ->
            { model | newCurrentGameName = Just "" }
                |> Cmd.with (Task.attempt (always NoOp) <| Dom.focus currentGameNameFieldId)

        SetNewCurrentGameName name ->
            ( { model | newCurrentGameName = Just name }, Cmd.none )

        SaveNewCurrentGameName ->
            { model | newCurrentGameName = Nothing }
                |> Cmd.with
                    (Maybe.unwrap Cmd.none
                        (Stream.sendMsg << Stream.RenameCurrentRound)
                        model.newCurrentGameName
                    )

        DiscardNewCurrentGameName ->
            ( { model | newCurrentGameName = Nothing }, Cmd.none )

        ToggleLeave ->
            ( { model | leaveConfirmation = not model.leaveConfirmation }
            , Cmd.none
            )



-- NoOp


handleEvent : Key -> Event -> Model -> ( Model, Cmd Msg )
handleEvent navigationKey event model =
    case event of
        PlayerJoin player ->
            ( updatePlayer player model
            , Cmd.none
            )

        PlayerStatusUpdate player ->
            ( updatePlayer player model
            , Cmd.none
            )

        SyncTableState ( table, game ) nextGameName ->
            ( { model
                | tableId = table.id
                , dealer = Just table.dealer.id
                , nextGameName = nextGameName
                , players =
                    List.map (\player -> ( player.id, player )) table.players
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

        VotingEnded game nextGameName ->
            ( { model
                | game = game
                , myVote = Nothing
                , nextGameName = nextGameName
              }
            , Cmd.none
            )

        GameEnded game ->
            ( { model | game = game }, Cmd.none )

        PlayerKicked player ->
            ( { model | players = Dict.remove player.id model.players }
            , if Just player.id == model.me then
                Navigation.pushUrl navigationKey "/"

              else
                Cmd.none
            )

        CurrentGameChanged game ->
            ( { model | game = game }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Stream.observe Event


viewTable : (Vote -> Int) -> (Maybe Vote -> Msg) -> Html Msg
viewTable toCount hover =
    Html.styled Html.div [ Css.marginLeft <| Css.px -12 ] [] <|
        Card.table toCount
            { click = Vote
            , hover = hover
            }


viewVoting : Model -> Html Msg
viewVoting model =
    let
        toCount vote =
            case model.myVote of
                Just myVote ->
                    if vote == myVote then
                        1

                    else
                        0

                Nothing ->
                    1
    in
    viewTable toCount (always NoOp)


viewPlayerVotes : Dict Int Vote -> Html Msg
viewPlayerVotes dict =
    let
        votes =
            Dict.values dict
                |> List.foldl (\v -> AnyDict.update v (Just << Maybe.unwrap 1 ((+) 1))) (AnyDict.empty Data.voteToInt)
    in
    viewTable (Maybe.withDefault 0 << flip AnyDict.get votes) HighlightVote


nameFieldId : String
nameFieldId =
    "game-name-field"


viewBigLabel : Bool -> String -> List (Html msg) -> Html msg
viewBigLabel shake txt under =
    Html.styled Html.div
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.height <| Css.px 400
        , Css.maxHeight <| Css.pct 100
        , Css.justifyContent Css.center
        , Css.alignItems Css.center
        ]
        []
        [ Html.styled Html.span
            [ Theme.highlight
            , Css.fontSize <| Css.px 40
            , Css.marginBottom <| Css.px 10
            , if shake then
                Theme.shaking 5

              else
                Css.batch []
            ]
            []
            [ Html.text txt ]
        , Html.styled Html.div [ Css.fontWeight <| Css.int 200 ] [] under
        ]


viewSetName : Model -> Html Msg
viewSetName model =
    let
        name =
            Maybe.withDefault "" model.gameName
    in
    if amIDealer model then
        Component.nameForm
            { onInput = SetName
            , onSubmit = NewGame name
            , submitTxt =
                if Data.isNewGame model.game then
                    "Start"

                else
                    "Next Task"
            , value = name
            , inputId = nameFieldId
            , labelTxt = "Name the task"
            , placeHolder = model.nextGameName
            , above =
                if Data.isNewGame model.game then
                    Html.styled Html.h2 [ Theme.heading ] [] [ Html.text "First Task" ]

                else
                    Html.styled Html.h2 [ Theme.heading ] [] [ Html.text "Next Task" ]
            , otherBtns =
                if Data.isNewGame model.game then
                    []

                else
                    [ Html.styled Html.button
                        [ Theme.secondaryBtn
                        , Css.marginTop <| Css.px 12
                        ]
                        [ Events.onClick ClearMyVote ]
                        [ Html.text "Back" ]
                    , Html.styled Html.button
                        [ Theme.secondaryBtn
                        , Css.marginTop <| Css.px 12
                        ]
                        [ Events.onClick FinishGame
                        , Attrs.type_ "button"
                        ]
                        [ Html.text "Finish Game" ]
                    ]
            , errorsView = Nothing
            }

    else
        viewBigLabel True "Wait till game starts..." []


viewOverviewTable : Dict Int { p | name : String } -> { b | playerVotes : List ( String, Dict Int Vote ), results : Dict String Vote } -> Html msg
viewOverviewTable players_ { playerVotes, results } =
    let
        toString =
            String.fromInt << Data.voteToInt

        voteCell votes name =
            Html.styled Html.td
                [ tdStyle ]
                []
                [ Html.text <| Maybe.unwrap "?" toString <| Dict.get name votes ]

        players =
            Dict.keys players_

        tdStyle =
            Css.batch
                [ Css.textAlign Css.center
                , Css.padding2 (Css.px 6) (Css.px 6)
                , Css.fontWeight <| Css.int 200
                ]

        thStyle =
            Css.batch
                [ tdStyle
                , Css.fontWeight <| Css.int 400
                ]

        highlighted =
            Css.batch
                [ tdStyle
                , Css.fontWeight <| Css.int 700
                ]

        viewTr ( name, votes ) =
            Html.styled Html.tr [ Css.borderTop3 (Css.px 1) Css.solid (Css.hex "#cccccc") ] [] <|
                [ Html.styled Html.td
                    [ highlighted
                    , Css.textAlign Css.left
                    ]
                    []
                    [ Html.text name ]
                , Html.styled Html.td [ highlighted ] [] <|
                    [ Html.text <|
                        Maybe.unwrap "?" toString <|
                            Dict.get name results
                    ]
                ]
                    ++ List.map (voteCell votes) players

        tBody =
            Html.tbody [] <|
                List.map viewTr playerVotes
    in
    Html.styled Html.table
        [ Css.minWidth <| Css.pct 100
        , Css.fontSize <| Css.px 16
        , Css.borderCollapse Css.collapse
        ]
        []
        [ Html.thead [] <|
            [ Html.tr [] <|
                Html.styled Html.th [ thStyle, Css.textAlign Css.left ] [] [ Html.text "Task" ]
                    :: Html.styled Html.th [ thStyle ] [] [ Html.text "Agreed" ]
                    :: List.map
                        (\id ->
                            Html.styled Html.th
                                [ thStyle ]
                                []
                                [ Html.text <| Maybe.unwrap "[unknown]" .name <| Dict.get id players_ ]
                        )
                        players
            ]
        , tBody
        ]


viewGame : Model -> Html Msg
viewGame model =
    let
        inner =
            case model.game of
                NotStarted ->
                    let
                        ( shake, text ) =
                            if amIDealer model then
                                ( True, "Start the game when ready..." )

                            else
                                ( False, "Prepare for start!" )
                    in
                    [ Theme.highlightedHeading shake [ Html.text text ]
                    , viewSetName model
                    ]

                Voting _ ->
                    let
                        amIVoting =
                            Maybe.andThen (flip Dict.get model.players) model.me
                                |> Maybe.unwrap False ((==) Active << .status)
                    in
                    (if amIVoting then
                        let
                            shake =
                                model.myVote == Nothing
                        in
                        [ Theme.highlightedHeading shake [ Html.text "Pick your card!" ]
                        , viewVoting model
                        , Html.br [] []
                        ]

                     else
                        [ viewBigLabel False
                            "You're not voting!"
                            [ Html.text "Wait for active players to finish their voting or "
                            , Html.styled Html.a
                                [ Css.fontWeight <| Css.int 400, Css.textDecoration Css.underline ]
                                [ Events.onClick <| Send <| Stream.ChangeStatus Active ]
                                [ Html.text "start voting" ]
                            , Html.text " yourself."
                            ]
                        ]
                    )
                        ++ (if amIDealer model then
                                [ Html.styled Html.button
                                    [ Theme.secondaryBtn ]
                                    [ Events.onClick <| Send Stream.FinishRound ]
                                    [ Html.text "Finish Round" ]
                                ]

                            else
                                []
                           )

                RoundFinished { playerVotes } ->
                    if amIDealer model && model.myVote /= Nothing then
                        -- Dealer definitng new ticket
                        [ Theme.highlightedHeading False [ Html.text "Set next task OR finish the game!" ]
                        , viewSetName model
                        ]

                    else if amIDealer model then
                        -- Dealer chossing agreed estimation
                        [ Theme.highlightedHeading True [ Html.text "Choose agreed extimation!" ]
                        , viewPlayerVotes playerVotes
                        , Html.br [] []
                        , Html.styled Html.button
                            [ Theme.secondaryBtn ]
                            [ Events.onClick <| Send Stream.Restart ]
                            [ Html.text "Restart Round" ]
                        ]

                    else
                        -- Player waiting for dealer
                        [ Theme.highlightedHeading False [ Html.text "Wait for the next ticket!" ]
                        , viewPlayerVotes playerVotes
                        ]

                Overview data ->
                    [ Html.styled Html.h2 [ Theme.heading, Css.marginTop Css.zero ] [] [ Html.text "Game results" ]
                    , viewOverviewTable model.players data
                    ]
    in
    Html.styled Html.div [ Css.width <| Css.px 835 ] [] inner


currentGameNameFieldId : String
currentGameNameFieldId =
    "curent-game-field-name-id"


viewCurrentGame : Bool -> Maybe String -> Game -> Html Msg
viewCurrentGame canEdit newName game =
    let
        headlineStyle =
            Css.batch
                [ Css.margin2 (Css.px 12) Css.zero
                , Css.fontSize <| Css.px 27
                ]

        headline =
            Html.styled Html.h2

        showName name =
            headline [ headlineStyle, Css.fontSize Css.inherit ]
                []
                [ Html.styled Html.span
                    [ Css.fontWeight <| Css.int 200
                    , Css.display Css.block
                    , Css.fontSize <| Css.px 20
                    , Css.marginBottom <| Css.px 6
                    ]
                    []
                    [ Html.text "Estimating task:" ]
                , case newName of
                    Nothing ->
                        Html.styled Html.div
                            [ Css.fontSize <| Css.px 27
                            , Css.position Css.relative
                            , Css.hover
                                [ Css.before <|
                                    if canEdit then
                                        [ Theme.stickyLabel
                                        , Css.property "content" "'Edit name'"
                                        ]

                                    else
                                        []
                                ]
                            ]
                            [ Events.onClick <|
                                if canEdit then
                                    EditCurrentGameName

                                else
                                    NoOp
                            ]
                            [ Html.text name ]

                    Just value ->
                        Html.form [ Events.onSubmit SaveNewCurrentGameName ]
                            [ Html.styled Html.input
                                [ Theme.textField ]
                                [ Events.onInput SetNewCurrentGameName
                                , Attrs.value value
                                , Attrs.id currentGameNameFieldId
                                ]
                                []
                            , Html.styled Html.button
                                [ Theme.primaryBtn
                                , Css.display Css.inlineBlock
                                , Css.marginRight <| Css.px 6
                                ]
                                [ Attrs.type_ "submit" ]
                                [ Html.text "save" ]
                            , Html.styled Html.button
                                [ Theme.secondaryBtn
                                , Css.display Css.inlineBlock
                                ]
                                [ Attrs.type_ "button", Events.onClick DiscardNewCurrentGameName ]
                                [ Html.text "cancel" ]
                            ]
                ]
    in
    case game of
        NotStarted ->
            headline
                [ headlineStyle
                , Css.fontWeight <| Css.int 200
                , Css.fontSize <| Css.px 20
                ]
                []
                [ Html.text "Game hasn't started." ]

        Voting { name } ->
            showName name

        RoundFinished { name } ->
            showName name

        Overview _ ->
            Html.text ""


viewPointsSoFar : Game -> Html msg
viewPointsSoFar game =
    let
        showPoints txt int =
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
                    [ Html.text txt ]
                , Html.text <| String.fromInt int
                ]
    in
    case game of
        NotStarted ->
            Html.text ""

        Voting { totalPoints } ->
            showPoints "Points so far:" totalPoints

        RoundFinished { totalPoints } ->
            showPoints "Points so far:" totalPoints

        Overview { totalPoints } ->
            showPoints "Total points:" totalPoints


editNameFieldId : String
editNameFieldId =
    "edit-player-name"


viewPlayerSetName : Maybe Player -> Maybe String -> Html Msg
viewPlayerSetName me newName =
    case newName of
        Just name ->
            Html.form [ Events.onSubmit SaveNewName ]
                [ Html.styled Html.input
                    [ Theme.textField ]
                    [ Events.onInput SetNewName
                    , Attrs.value name
                    , Attrs.id editNameFieldId
                    ]
                    []
                , Html.styled Html.button
                    [ Theme.primaryBtn
                    , Css.display Css.inlineBlock
                    , Css.marginRight <| Css.px 6
                    ]
                    [ Attrs.type_ "submit" ]
                    [ Html.text "save" ]
                , Html.styled Html.button
                    [ Theme.secondaryBtn
                    , Css.display Css.inlineBlock
                    ]
                    [ Attrs.type_ "button", Events.onClick DiscardNewName ]
                    [ Html.text "cancel" ]
                ]

        Nothing ->
            Html.div []
                [ Html.styled Html.h3
                    [ Css.position Css.relative
                    , Css.margin2 (Css.px 2) Css.zero
                    , Css.marginBottom <| Css.px 0
                    , Css.fontWeight <| Css.int 200
                    , Css.hover
                        [ Css.before
                            [ Theme.stickyLabel
                            , Css.property "content" "'Edit name'"
                            ]
                        ]
                    ]
                    [ Events.onClick <| EditName ]
                    [ Html.text <| Maybe.unwrap "" .name me ]
                ]


viewMe : Model -> Html Msg
viewMe { me, dealer, newName, players, game, leaveConfirmation } =
    let
        mePlayer =
            Maybe.andThen (flip Dict.get players) me

        active =
            Maybe.map .status mePlayer == Just Active

        footerItem =
            Css.batch
                [ Css.display Css.block
                , Css.fontSize <| Css.px 14
                , Css.marginTop <| Css.px 12
                ]
    in
    Html.styled Html.div
        [ Css.margin3 (Css.px 10) Css.zero (Css.px 20)
        , Css.paddingBottom <| Css.px 10
        , Css.borderBottom3 (Css.px 2) Css.dotted <| Css.hex "c0c0c0c0"
        ]
        []
        [ Html.styled Html.span
            [ Css.fontWeight <| Css.int 400
            , Css.fontSize <| Css.px 14
            ]
            []
            [ Html.text "Playing as:" ]
        , Html.styled Html.div
            [ Css.marginBottom <| Css.px 20 ]
            []
            [ viewPlayerSetName mePlayer newName ]
        , if Data.isOverview game then
            Html.text ""

          else
            Html.styled Html.a
                [ footerItem
                , Css.color Theme.values.darkColor
                , Css.fontWeight <| Css.int 200
                ]
                [ Events.onClick <|
                    Send <|
                        Stream.ChangeStatus <|
                            if active then
                                Idle

                            else
                                Active
                ]
                [ Theme.toggle active
                , Html.text <|
                    if active then
                        "Stop voting"

                    else
                        "Start voting"
                ]
        , if me == dealer || Data.votingEnded game then
            Html.text ""

          else
            Html.styled Html.div
                [ footerItem ]
                []
                [ Html.styled
                    Html.div
                    (if leaveConfirmation then
                        [ Theme.selectedBox
                        , Css.backgroundColor Theme.values.lightRedBackground
                        ]

                     else
                        []
                    )
                    []
                    [ Html.styled Html.a
                        [ Css.display Css.inlineBlock
                        , Css.fontSize <| Css.px 14
                        , Css.fontWeight <| Css.int 400
                        , Css.color <|
                            if leaveConfirmation then
                                Theme.values.darkColor

                            else
                                Theme.values.primaryColor
                        , Css.textDecoration Css.underline
                        , Css.cursor Css.pointer
                        ]
                        [ Events.onClick ToggleLeave ]
                        [ Html.text "Leave table" ]
                    , if leaveConfirmation then
                        Html.div []
                            [ Html.styled Html.div
                                [ Css.fontWeight <| Css.int 200
                                , Css.marginTop <| Css.px 4
                                , Css.fontSize <| Css.px 12
                                ]
                                []
                                [ Html.text "This action will remove all your votes." ]
                            , Html.styled Html.button
                                [ Theme.secondaryBtn
                                , Css.fontSize <| Css.px 11
                                , Css.marginTop <| Css.px 4
                                ]
                                (Maybe.unwrap []
                                    (List.singleton
                                        << Events.onClick
                                        << Send
                                        << Stream.KickPlayer
                                    )
                                    (Maybe.andThen (flip Dict.get players) me)
                                )
                                [ Html.text "Proceed Anyway" ]
                            , Html.styled Html.button
                                [ Theme.primaryBtn
                                , Css.fontSize <| Css.px 11
                                , Css.marginTop <| Css.px 4
                                ]
                                [ Events.onClick ToggleLeave ]
                                [ Html.text "Cancel" ]
                            ]

                      else
                        Html.text ""
                    ]
                ]
        ]


view : Model -> Html Msg
view model =
    Component.withTableNotFound model.tableError <|
        Html.styled Html.div
            [ Css.displayFlex
            , Css.justifyContent Css.spaceBetween
            ]
            []
            [ viewGame model
            , Html.styled Html.aside
                [ Css.width <| Css.px 250 ]
                []
                [ viewCurrentGame (amIDealer model) model.newCurrentGameName model.game
                , viewPointsSoFar model.game
                , viewMe model
                , Players.view
                    { isMe =
                        \{ id } ->
                            Maybe.unwrap False ((==) id) model.me
                    , isDealer =
                        \{ id } -> Maybe.unwrap False ((==) id) model.dealer
                    , toVote =
                        \player ->
                            case model.game of
                                NotStarted ->
                                    Hidden

                                Voting { maskedVotes } ->
                                    if Set.member player.id maskedVotes then
                                        Unknown

                                    else
                                        Hidden

                                RoundFinished { playerVotes } ->
                                    Dict.get player.id playerVotes
                                        |> Maybe.unwrap Unknown Voted

                                Overview _ ->
                                    Hidden
                    , select = FocusPlayer << Just << .id
                    , deselect = FocusPlayer Nothing
                    , kickPlayer = Send << Stream.KickPlayer
                    }
                    (amIDealer model)
                    model.focusedPlayer
                    model.highlightedVote
                    model.players
                ]
            ]
