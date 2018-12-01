module Join exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Navigation exposing (Key)
import Data exposing (Session, Table)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Http
import Url.Builder as Url


type alias Model =
    { userName : String
    }


init : ( Model, Cmd msg )
init =
    ( Model ""
    , Cmd.none
    )


type Msg
    = UpdateName String
    | Submit
    | JoinResponse (Result Http.Error Table)


update :
    (Session -> (Result Http.Error Table -> Msg) -> String -> Cmd Msg)
    -> Key
    -> Session
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update action navigationKey session msg model =
    case msg of
        UpdateName str ->
            ( Model str, Cmd.none )

        Submit ->
            ( model
              -- or join table
            , action session JoinResponse model.userName
            )

        JoinResponse res ->
            case res of
                Ok { id } ->
                    ( model
                    , Navigation.pushUrl navigationKey <|
                        Url.absolute [ "table", id ] []
                    )

                Err _ ->
                    -- @TODO: clear session
                    ( model, Cmd.none )


view : Model -> Html Msg
view { userName } =
    Html.form
        [ Event.onSubmit Submit ]
        [ Html.input
            [ Attr.value userName
            , Event.onInput UpdateName
            ]
            []
        , Html.button [ Attr.type_ "submit" ]
            [ Html.text "Submit" ]
        , Html.a [ Attr.href "/some-room" ] [ Html.text "some room" ]
        ]