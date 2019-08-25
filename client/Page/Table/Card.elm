module Page.Table.Card exposing (table)

import Css
import Css.Global as GCss
import Css.Transitions as Transitions
import Data exposing (Vote(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events


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


view : (Vote -> Int) -> (Vote -> msg) -> Vote -> Html msg
view toCount msg vote =
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

        singleCard i =
            Html.styled Html.div
                [ Css.position Css.absolute
                , Css.top Css.zero
                , Css.left <| Css.px (2 * toFloat i)
                , Css.width <| Css.pct 100
                , Css.height <| Css.pct 100
                , Css.transformStyle Css.preserve3d
                , Css.boxShadow4
                    (Css.px <|
                        if toCount vote > 0 then
                            -1

                        else
                            1
                    )
                    (Css.px -1)
                    (Css.px 5)
                  <|
                    Css.rgba 0 0 0 0.3
                , Css.transforms
                    [ Css.rotateY <|
                        Css.deg <|
                            if toCount vote > 0 then
                                0

                            else
                                180
                    , Css.rotateZ <|
                        Css.deg (toFloat i * 2)
                    ]
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
    in
    Html.styled Html.button
        [ Css.position Css.relative
        , Css.transform <| Css.perspective 1000
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
    <|
        if toCount vote > 0 then
            List.indexedMap (\i _ -> singleCard i) <|
                List.range 1 <|
                    min (toCount vote) 5

        else
            [ singleCard 0 ]


table : (Vote -> Int) -> (Vote -> msg) -> List (Html msg)
table toCount msg =
    [ view toCount msg OnePoint
    , view toCount msg TwoPoints
    , view toCount msg ThreePoints
    , view toCount msg FivePoints
    , view toCount msg EightPoints
    , view toCount msg ThreeteenPoints
    , view toCount msg TwentyPoints
    , view toCount msg FortyPoints
    , view toCount msg HundredPoints
    , view toCount msg InfinityPoints
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
