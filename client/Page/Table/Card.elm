module Page.Table.Card exposing (Side(..), table)

import Css
import Css.Global as GCss
import Css.Transitions as Transitions
import Data exposing (Vote(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events


type Side
    = Front
    | Back


cardBackground : Vote -> String
cardBackground vote =
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
    in
    "/svg/card-" ++ name ++ ".svg"


view : (Vote -> Side) -> (Vote -> msg) -> Vote -> Html msg
view toSide msg vote =
    let
        transitionMs =
            600

        radius =
            Css.px 12

        cardStyles =
            Css.batch
                [ Css.property "content" "''"
                , Css.display Css.block
                , Css.position Css.absolute
                , Css.top Css.zero
                , Css.left Css.zero
                , Css.width <| Css.pct 100
                , Css.height <| Css.pct 100
                , Css.borderRadius radius
                , Css.property "-webkit-backface-visibility" "hidden"
                , Css.property "backface-visibility" "hidden"
                , Css.backgroundSize2 Css.auto <| Css.pct 100
                , Css.backgroundPosition Css.center
                ]
    in
    Html.styled Html.button
        [ Css.transform <| Css.perspective 1000
        , Css.width <| Css.px 144
        , Css.height <| Css.px 224
        , Css.margin <| Css.px 12
        , Css.padding Css.zero
        , Css.border Css.zero
        , Css.outline Css.zero
        , Css.cursor Css.pointer
        , Css.backgroundColor Css.transparent
        ]
        [ Events.onClick <| msg vote
        , Attrs.class "card"
        ]
        [ Html.styled Html.div
            [ Css.position Css.relative
            , Css.width <| Css.pct 100
            , Css.height <| Css.pct 100
            , Css.transformStyle Css.preserve3d
            , Css.boxShadow4
                (Css.px <|
                    if toSide vote == Front then
                        -1

                    else
                        1
                )
                (Css.px -1)
                (Css.px 5)
              <|
                Css.rgba 0 0 0 0.3
            , Css.transform <|
                Css.rotateY <|
                    Css.deg <|
                        if toSide vote == Front then
                            0

                        else
                            180
            , Css.borderRadius radius
            , Transitions.transition
                [ Transitions.textShadow transitionMs
                , Transitions.transform transitionMs
                ]
            , Css.before <|
                [ cardStyles
                , Css.backgroundImage <| Css.url <| cardBackground vote
                , Css.zIndex <| Css.int 2
                , Css.transform <| Css.rotateY <| Css.deg 0
                ]
            , Css.after <|
                [ cardStyles
                , Css.backgroundImage <| Css.url <| "/svg/card-cover.svg"
                , Css.transform <| Css.rotateY <| Css.deg -180
                ]
            ]
            [ Attrs.class "inner-card" ]
            []
        ]


table : (Vote -> Side) -> (Vote -> msg) -> List (Html msg)
table toSide msg =
    [ view toSide msg OnePoint
    , view toSide msg TwoPoints
    , view toSide msg ThreePoints
    , view toSide msg FivePoints
    , view toSide msg EightPoints
    , view toSide msg ThreeteenPoints
    , view toSide msg TwentyPoints
    , view toSide msg FortyPoints
    , view toSide msg HundredPoints
    , view toSide msg InfinityPoints
    , GCss.global
        [ GCss.selector ".card:hover .inner-card"
            [ Css.transforms
                [ Css.rotateY <| Css.deg 0
                , Css.scale 1.1
                ]
            , Css.boxShadow4 (Css.px -4) (Css.px -7) (Css.px 20) <| Css.rgba 0 0 0 0.2
            , Css.zIndex <| Css.int 2
            ]
        ]
    ]
