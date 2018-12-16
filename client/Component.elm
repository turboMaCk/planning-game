module Component exposing (withTableNotFound)

import Data exposing (ApiError, TableError(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Maybe.Extra as Maybe
import Url.Builder as Url


withTableNotFound : Maybe (ApiError TableError) -> Html msg -> Html msg
withTableNotFound tableError view =
    if Maybe.isJust <| Maybe.filter (Data.errorIs TableNotFound) tableError then
        Html.div []
            [ Html.text "Table doesn't exist"
            , Html.br [] []
            , Html.a [ Attr.href <| Url.absolute [] [] ]
                [ Html.text "Continue on Homepage" ]
            ]

    else
        view
