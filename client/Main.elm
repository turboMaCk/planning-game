module Main exposing (main)

import Browser exposing (Document)
import Data
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Http
import Stream exposing (Event(..))
import Url exposing (Url)


type View
    = SetName


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
    ( { view = SetName, userName = "", sessionId = Ok sessionId }
    , Cmd.none
    )


type Msg
    = NoOp
    | UpdateName String
    | SubmitName
    | SessionCreated (Result Http.Error String)


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
            ( { model | sessionId = Result.map Just res }
            , Result.map Stream.connect res
                |> Result.withDefault Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.sessionId of
        Ok (Just _) ->
            Stream.observe (always NoOp)

        _ ->
            Sub.none



-- View


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
    , body =
        case model.view of
            SetName ->
                setNameView model.userName
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
