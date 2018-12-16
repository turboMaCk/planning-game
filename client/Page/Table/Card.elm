module Page.Table.Card exposing (view)

import Css
import Css.Transitions as Transitions
import Data exposing (Vote(..))
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events


getSvgPath : Vote -> String
getSvgPath vote =
    let
        name =
            case vote of
                OnePoint ->
                    "1"

                TwoPoints ->
                    "2"

                ThreePoints ->
                    "3"

                FivePoints ->
                    "5"

                EightPoints ->
                    "8"

                ThreeteenPoints ->
                    "13"

                TwentyPoints ->
                    "20"

                FortyPoints ->
                    "40"

                HundredPoints ->
                    "100"

                InfinityPoints ->
                    "infinity"

                UnknownPoints ->
                    "unknown"
    in
    "/svg/card-" ++ name ++ ".svg"


view : (Vote -> msg) -> Vote -> Html msg
view msg vote =
    Html.styled Html.button
        [ Css.width <| Css.px 155
        , Css.height <| Css.px 231
        , Css.margin <| Css.px 6
        , Css.padding Css.zero
        , Css.border Css.zero
        , Css.cursor Css.pointer
        , Css.borderRadius <| Css.px 4
        , Css.backgroundImage <| Css.url <| getSvgPath vote
        , Css.boxShadow4 (Css.px 1) (Css.px 2) (Css.px 4) <| Css.rgba 0 0 0 0.3
        , Transitions.transition
            [ Transitions.textShadow 200
            , Transitions.transform 200
            ]
        , Css.hover
            [ Css.boxShadow4 (Css.px 3) (Css.px 5) (Css.px 7) <| Css.rgba 0 0 0 0.3
            , Css.transform <| Css.scale 1.05
            ]
        ]
        [ Events.onClick <| msg vote
        ]
        []
