module Page.Table.Card exposing (view)

import Data exposing (Vote(..))
import Html exposing (Attribute, Html)
import Html.Attributes as Attrs
import Html.Events as Events


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
    "url('/svg/card-" ++ name ++ ".svg')"


view : (Vote -> msg) -> Vote -> Html msg
view msg vote =
    Html.button
        [ Events.onClick <| msg vote
        , Attrs.class "card"
        , Attrs.style "width" "155px"
        , Attrs.style "height" "231px"
        , Attrs.style "margin" "6px"
        , Attrs.style "padding" "0"
        , Attrs.style "border" "0"
        , Attrs.style "outline" "0"
        , Attrs.style "cursor" "pointer"
        , Attrs.style "transition" "all .2s"
        , Attrs.style "border-radius" "4px"
        , Attrs.style "box-shadow" "1px 2px 3px rgba(0,0,0,.3)"
        , Attrs.style "background" <| getSvgPath vote
        ]
        []
