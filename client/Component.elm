module Component exposing (nameForm, withTableNotFound)

import Css
import Data exposing (ApiError, TableError(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Maybe.Extra as Maybe
import Theme
import Url.Builder as Url


withTableNotFound : Maybe (ApiError TableError) -> Html msg -> Html msg
withTableNotFound tableError view =
    if Maybe.isJust <| Maybe.filter (Data.errorIs TableNotFound) tableError then
        Html.div []
            [ Html.text "Table doesn't exist"
            , Html.br [] []
            , Html.a [ Attrs.href <| Url.absolute [] [] ]
                [ Html.text "Continue on Homepage" ]
            ]

    else
        view


nameForm :
    { onInput : String -> msg
    , onSubmit : msg
    , submitTxt : String
    , value : String
    , inputId : String
    , labelTxt : String
    , above : Html msg
    , otherBtns : List (Html msg)
    , errorsView : Maybe (Html msg)
    }
    -> Html msg
nameForm { onInput, onSubmit, submitTxt, value, inputId, labelTxt, above, otherBtns, errorsView } =
    Html.styled Html.form
        [ Css.width <| Css.px 300
        , Css.margin2 (Css.px 150) Css.auto
        ]
        [ Events.onSubmit onSubmit
        ]
        ([ above
         , Html.label [ Attrs.for inputId ] [ Html.text labelTxt ]
         , Html.styled Html.input
            [ Theme.textField ]
            [ Events.onInput onInput
            , Attrs.id inputId
            , Attrs.value value
            ]
            []
         , Maybe.withDefault (Html.text "") errorsView
         , Html.styled Html.button
            [ Theme.primaryBtn ]
            [ Attrs.type_ "submit"
            ]
            [ Html.text submitTxt ]
         ]
            ++ otherBtns
        )
