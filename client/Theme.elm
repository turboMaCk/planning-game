module Theme exposing
    ( globalStyles
    , heading
    , highlight
    , highlightedHeading
    , logo
    , pill
    , primaryBtn
    , secondaryBtn
    , shaking
    , stickyLabel
    , textField
    , toggle
    , values
    )

import Css exposing (Style)
import Css.Animations as Animations
import Css.Global as GCss
import Css.Transitions as Transitions
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Url.Builder as Url


values =
    { mainFontFamily = [ "Roboto Slab", "serif" ]
    , primaryColor = Css.hex "#ff495f"
    , secondaryColor = Css.hex "#3280ff"
    , darkColor = Css.hex "#000000"
    , lightColor = Css.hex "#ffffff"
    , lightBackground = Css.hex "#d6e5ff"
    , greenColor = Css.hex "#8fdb41"
    }


logo : Html msg
logo =
    Html.h1 []
        [ Html.styled Html.a
            [ Css.cursor Css.pointer
            , Css.color values.darkColor
            , Css.textDecoration Css.none
            ]
            [ Attrs.href <| Url.absolute [] [] ]
            [ Html.styled Html.span
                [ Css.fontWeight <| Css.int 500 ]
                []
                [ Html.styled Html.span
                    [ Css.display Css.inlineBlock
                    , Css.backgroundColor values.primaryColor
                    , Css.color values.lightColor
                    , Css.transform <| Css.rotate <| Css.deg 6
                    , Css.padding2 Css.zero <| Css.px 1
                    ]
                    []
                    [ Html.text "P" ]
                , Html.text "lanning "
                ]
            , Html.text "Game"
            ]
        ]


btnMixin : Style
btnMixin =
    Css.batch
        [ Css.display Css.block
        , Css.padding Css.zero
        , Css.fontSize <| Css.px 17
        , Css.border Css.zero
        , Css.outline Css.zero
        , Css.cursor Css.pointer
        , Css.backgroundColor Css.transparent
        ]


primaryBtn : Style
primaryBtn =
    Css.batch
        [ btnMixin
        , Css.borderBottom3 (Css.px 2) Css.solid values.secondaryColor
        ]


secondaryBtn : Style
secondaryBtn =
    Css.batch
        [ btnMixin
        , Css.fontWeight <| Css.int 200
        , Css.borderBottom3 (Css.px 2) Css.solid values.primaryColor
        ]


heading : Style
heading =
    Css.batch
        [ Css.margin2 (Css.px 12) Css.zero
        , Css.fontSize <| Css.px 27
        , Css.fontWeight <| Css.int 200
        ]


highlight : Style
highlight =
    Css.batch
        [ Css.color values.lightColor
        , Css.backgroundColor values.secondaryColor
        ]


highlightedHeading : Bool -> List (Html msg) -> Html msg
highlightedHeading shake inner =
    Html.styled Html.h2
        (if shake then
            [ shaking 0
            , heading
            , Css.display Css.inlineBlock
            , Css.marginTop Css.zero
            ]

         else
            [ heading
            , Css.marginTop Css.zero
            ]
        )
        []
    <|
        [ Html.styled Html.span
            [ highlight
            , Css.fontSize <| Css.px 20
            ]
            []
            inner
        ]


textField : Style
textField =
    Css.batch
        [ Css.border Css.zero
        , Css.width <| Css.px 240
        , Css.lineHeight <| Css.px 25
        , Css.fontSize <| Css.px 17
        , Css.outline Css.zero
        , Css.fontWeight <| Css.int 700
        , Css.borderBottom3 (Css.px 3) Css.solid values.secondaryColor
        , Css.backgroundColor values.lightBackground
        , Css.pseudoElement "placeholder"
            [ Css.color values.darkColor
            , Css.fontWeight <| Css.int 400
            ]
        ]


pill : Style
pill =
    Css.batch
        [ Css.display Css.inlineBlock
        , Css.minWidth <| Css.px 8
        , Css.margin2 Css.zero <| Css.px 4
        , Css.padding2 Css.zero <| Css.px 4
        , Css.fontSize <| Css.px 12
        , Css.fontWeight <| Css.int 700
        , Css.color values.lightColor
        , Css.textAlign Css.center
        ]


globalStyles : Html msg
globalStyles =
    GCss.global
        [ GCss.html
            [ Css.height <| Css.pct 100 ]
        , GCss.body
            [ Css.position Css.relative
            , Css.minHeight <| Css.pct 100
            , Css.margin Css.zero
            , Css.padding4 (Css.px 6) Css.zero (Css.px 200) Css.zero
            , Css.boxSizing Css.borderBox
            ]
        , GCss.each [ GCss.input, GCss.body, GCss.button ]
            [ Css.fontFamilies values.mainFontFamily ]
        , GCss.button
            [ Css.cursor Css.pointer ]
        , GCss.a
            [ Css.color values.secondaryColor
            , Css.cursor Css.pointer
            ]
        , GCss.each [ GCss.p, GCss.input, GCss.label ]
            [ Css.fontWeight <| Css.int 200
            , Css.margin2 (Css.px 12) Css.zero
            ]
        ]


shaking : Float -> Style
shaking rot =
    let
        rotate =
            List.singleton
                << Animations.transform
                << List.singleton
                << Css.rotate
                << Css.deg

        keyframes =
            Animations.keyframes
                [ ( 0, rotate rot )
                , ( 90, rotate rot )
                , ( 92, rotate <| rot + 3 )
                , ( 94, rotate <| rot - 3 )
                , ( 96, rotate <| rot + 3 )
                , ( 98, rotate <| rot - 3 )
                , ( 100, rotate rot )
                ]
    in
    Css.batch
        [ Css.animationName keyframes
        , Css.animationDuration <| Css.ms 5000
        , Css.property "animation-iteration-count" "infinite"
        ]


stickyLabel : Style
stickyLabel =
    Css.batch
        [ Css.position Css.absolute
        , Css.fontSize <| Css.px 11
        , Css.color <| values.secondaryColor
        , Css.backgroundColor <| Css.hex "ffffff"
        , Css.top <| Css.pct 100
        , Css.textDecoration Css.underline
        , Css.marginTop <| Css.px -2
        , Css.fontWeight <| Css.int 200
        ]


toggle : Bool -> Html msg
toggle active =
    let
        height =
            15

        width =
            30

        padding =
            2
    in
    Html.styled Html.div
        [ Css.position Css.relative
        , Css.display Css.inlineBlock
        , Css.backgroundColor <| Css.hex "ececec"
        , Css.margin4 Css.zero (Css.px 3) (Css.px -3) Css.zero
        , Css.width <| Css.px width
        , Css.height <| Css.px height
        , Css.borderRadius <| Css.px (height / 2)
        , Css.cursor Css.pointer
        , Css.before
            [ Css.property "content" "''"
            , Css.display Css.block
            , Css.position Css.absolute
            , Css.width <| Css.px (height - padding * 2)
            , Css.height <| Css.px (height - padding * 2)
            , Css.borderRadius <| Css.pct 100
            , Css.marginTop <| Css.px padding
            , Css.backgroundColor <|
                if active then
                    values.secondaryColor

                else
                    values.primaryColor
            , Css.left <|
                if active then
                    Css.px padding

                else
                    Css.px (width - height + padding)
            , Transitions.transition [ Transitions.left 500, Transitions.background 250 ]
            ]
        ]
        []
        []
