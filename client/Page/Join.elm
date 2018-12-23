module Page.Join exposing (Model, Msg(..), init, update, view)

import Browser.Dom as Dom
import Browser.Navigation as Navigation exposing (Key)
import Component
import Css
import Data exposing (ApiError, Session, Table, TableError(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Http
import Task
import Theme
import Url.Builder as Url


type alias Model =
    { playerName : String
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
            ( { model | playerName = str }, Cmd.none )

        Submit ->
            ( model
            , action session JoinResponse model.playerName
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
    Html.styled Html.div
        [ Css.color Theme.values.primaryColor
        , Css.marginBottom <| Css.px 6
        ]
        []
        [ Html.text <| Data.errorMessage err ]


fieldId : String
fieldId =
    "join-name-field"


view : Html Msg -> Model -> Html Msg
view headline { playerName, tableError } =
    Component.withTableNotFound tableError <|
        Html.div []
            [ Component.nameForm
                { onInput = UpdateName
                , onSubmit = Submit
                , submitTxt = "Submit"
                , value = playerName
                , inputId = fieldId
                , labelTxt = "Choose your name for the table"
                , above = headline
                , otherBtns = []
                , errorsView = Maybe.map viewError tableError
                }
            ]
