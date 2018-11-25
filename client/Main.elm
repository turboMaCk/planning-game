module Main exposing (main)

import Browser exposing (Document)
import Data exposing (User)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Http
import Json.Decode as Decode
import Stream exposing (Event(..))
import Task
import Url exposing (Url)


type alias GameModel =
    { players : List User
    }


type View
    = SetName
    | Game GameModel


type alias Model =
    { view : View
    , userName : String
    , sessionId : Result Http.Error (Maybe String)
    }


type alias Flags =
    { sessionId : Maybe String
    }


init : Flags -> Url -> key -> ( Model, Cmd Msg )
init { sessionId } _ _ =
    let
        ( viewModel, cmd ) =
            case sessionId of
                Just sId ->
                    ( Game { players = [] }
                    , Ok sId
                        |> Result.map Stream.connect
                        |> Result.withDefault Cmd.none
                    )

                Nothing ->
                    ( SetName, Cmd.none )
    in
    ( { view = viewModel, userName = "", sessionId = Ok sessionId }
    , cmd
    )


type Msg
    = NoOp
    | UpdateName String
    | SubmitName
    | SessionCreated (Result Http.Error String)
    | StreamEvent (Result Decode.Error Event)
    | ClearSession


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateName str ->
            ( { model | userName = str }
            , Cmd.none
            )

        SubmitName ->
            ( model
            , Data.setName SessionCreated model.userName
            )

        SessionCreated res ->
            ( { model
                | sessionId = Result.map Just res
                , view = Game <| GameModel []
              }
            , Result.map Stream.connect res
                -- @TODO: replace with cmd-extra?
                |> Result.withDefault (Task.perform identity <| Task.succeed ClearSession)
            )

        ClearSession ->
            ( { model
                | sessionId = Ok Nothing
                , view = SetName
              }
            , Stream.disconnect ()
            )

        StreamEvent result ->
            case result of
                Ok (UserJoin newUser) ->
                    case model.view of
                        Game { players } ->
                            ( { model
                                | view = Game { players = newUser :: players }
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    let
                        _ =
                            Debug.log "err" result
                    in
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.sessionId of
        Ok (Just _) ->
            Stream.observe StreamEvent

        _ ->
            Sub.none



-- View


viewUser : User -> Html msg
viewUser { name, isConnected } =
    Html.li []
        [ Html.span []
            [ if isConnected then
                Html.text "o"

              else
                Html.text "x"
            ]
        , Html.text " "
        , Html.text name
        ]


gameView : GameModel -> List (Html Msg)
gameView { players } =
    [ Html.aside []
        [ Html.ul [] <|
            List.map viewUser players
        ]
    ]


setNameView : String -> List (Html Msg)
setNameView name =
    [ Html.form
        [ Event.onSubmit SubmitName ]
        [ Html.input
            [ Attr.value name
            , Event.onInput UpdateName
            ]
            []
        , Html.button [ Attr.type_ "submit" ]
            [ Html.text "Submit" ]
        ]
    ]


view : Model -> Document Msg
view model =
    { title =
        case model.view of
            SetName ->
                "Join | Agile Poker"

            Game _ ->
                "Game | Agile Poker"
    , body =
        case model.view of
            SetName ->
                setNameView model.userName

            Game m ->
                gameView m
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        }
