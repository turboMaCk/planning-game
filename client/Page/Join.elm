module Page.Join exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Navigation exposing (Key)
import Component
import Data exposing (ApiError, Session, Table, TableError(..))
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
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
        Html.div
            [ Attrs.style "width" "300px"
            , Attrs.style "margin" "150px auto"
            ]
            [ Html.p [ Attrs.style "font-weight" "200" ] [ Html.text "Choose your player name." ]
            , Html.form
                [ Events.onSubmit Submit
                ]
                [ Html.input
                    [ Events.onInput UpdateName
                    , Attrs.value userName
                    , Attrs.style "border" "0"
                    , Attrs.style "width" "230px"
                    , Attrs.style "line-height" "25px"
                    , Attrs.style "font-size" "17px"
                    , Attrs.style "outline" "0"
                    , Attrs.style "font-weight" "600"
                    , Attrs.style "border-bottom" "3px solid #3280ff"
                    , Attrs.style "background" "#d6e5ff"
                    ]
                    []
                , Html.button
                    [ Attrs.type_ "submit"
                    , Attrs.style "display" "block"
                    , Attrs.style "border" "0"
                    , Attrs.style "font-size" "17px"
                    , Attrs.style "border-bottom" "2px solid red"
                    , Attrs.style "padding" "0"
                    , Attrs.style "margin-top" "12px"
                    , Attrs.style "outline" "0"
                    , Attrs.style "cursor" "pointer"
                    ]
                    [ Html.text "Submit" ]
                , case tableError of
                    Just err ->
                        viewError err

                    Nothing ->
                        Html.text ""
                ]
            ]
