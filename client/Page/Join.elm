module Page.Join exposing (Model, Msg(..), init, update, view)

import Browser.Dom as Dom
import Browser.Navigation as Navigation exposing (Key)
import Component
import Css
import Data exposing (ApiError, Session, Table, TableError(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Http
import Task
import Theme
import Url.Builder as Url


type alias Model =
    { userName : String
    , tableError : Maybe (ApiError TableError)
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" Nothing
    , Task.attempt (always NoOp) <| Dom.focus fieldId
    )


type Msg
    = NoOp
    | UpdateName String
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
        NoOp ->
            ( model, Cmd.none )

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


fieldId : String
fieldId =
    "join-name-field"


view : Model -> Html Msg
view { userName, tableError } =
    Component.withTableNotFound tableError <|
        Html.styled Html.div
            [ Css.width <| Css.px 300
            , Css.margin2 (Css.px 150) Css.auto
            ]
            []
            [ Html.p [] [ Html.text "Choose your player name." ]
            , Html.form
                [ Events.onSubmit Submit
                ]
                [ Html.styled Html.input
                    Theme.textField
                    [ Events.onInput UpdateName
                    , Attrs.id fieldId
                    , Attrs.value userName
                    ]
                    []
                , Html.styled Html.button
                    Theme.primaryBtn
                    [ Attrs.type_ "submit"
                    ]
                    [ Html.text "Submit" ]
                , case tableError of
                    Just err ->
                        viewError err

                    Nothing ->
                        Html.text ""
                ]
            ]
