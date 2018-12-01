module Join exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Navigation exposing (Key)
import Component
import Data exposing (ApiError, Session, Table, TableError(..))
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Http
import Url.Builder as Url


type alias Model =
    { userName : String
    , tableError : Maybe (ApiError TableError)
    }


init : ( Model, Cmd msg )
init =
    ( Model "" Nothing
    , Cmd.none
    )


type Msg
    = UpdateName String
    | Submit
    | JoinResponse (Result (ApiError TableError) Table)


update :
    (Session -> (Result (ApiError TableError) Table -> Msg) -> String -> Cmd Msg)
    -> Key
    -> Session
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update action navigationKey session msg model =
    case msg of
        UpdateName str ->
            ( { model | userName = str }, Cmd.none )

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

                Err err ->
                    ( { model | tableError = Just <| Debug.log "Err" err }, Cmd.none )


viewError : ApiError TableError -> Html msg
viewError err =
    Html.div []
        [ Html.text <| Data.errorMessage err ]


view : Model -> Html Msg
view { userName, tableError } =
    Component.withTableNotFound tableError <|
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
            , case tableError of
                Just err ->
                    viewError err

                Nothing ->
                    Html.text ""
            ]
