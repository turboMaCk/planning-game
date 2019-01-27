module Theme exposing
    ( globalStyles
    , heading
    , highlightedHeading
    , logo
    , pill
    , primaryBtn
    , secondaryBtn
    , textField
    , values
    )

import Css exposing (Style)
import Css.Global as GCss
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Url.Builder as Url


values =
    { mainFontFamily = [ "Roboto Slab", "serif" ]
    , primaryColor = Css.hex "#ff495f"
    , secondaryColor = Css.hex "#3280ff"
    , darkColor = Css.hex "#000000"
    , lightColor = Css.hex "#ffffff"
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


highlightedHeading : List (Html msg) -> Html msg
highlightedHeading inner =
    Html.styled Html.h2 [ heading, Css.marginTop Css.zero ] [] <|
        [ Html.styled Html.span
            [ Css.color values.lightColor
            , Css.backgroundColor values.secondaryColor
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
        , Css.backgroundColor <| Css.hex "#d6e5ff"
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
            [ Css.color values.secondaryColor ]
        , GCss.each [ GCss.p, GCss.input, GCss.label ]
            [ Css.fontWeight <| Css.int 200
            , Css.margin2 (Css.px 12) Css.zero
            ]
        ]
