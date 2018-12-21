module Theme exposing (globalStyles, logo, primaryBtn, textField, values)

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
                    [ Css.backgroundColor values.primaryColor
                    , Css.color values.lightColor
                    ]
                    []
                    [ Html.text "A" ]
                , Html.text "gile "
                ]
            , Html.styled Html.span
                [ Css.borderBottom3 (Css.px 3) Css.solid values.primaryColor ]
                []
                [ Html.text "Poker" ]
            ]
        ]


primaryBtn : List Style
primaryBtn =
    [ Css.display Css.block
    , Css.padding Css.zero
    , Css.fontSize <| Css.px 17
    , Css.border Css.zero
    , Css.borderBottom3 (Css.px 2) Css.solid values.primaryColor
    , Css.outline Css.zero
    , Css.cursor Css.pointer
    , Css.backgroundColor Css.transparent
    ]


textField : List Style
textField =
    [ Css.border Css.zero
    , Css.width <| Css.px 230
    , Css.lineHeight <| Css.px 25
    , Css.fontSize <| Css.px 17
    , Css.outline Css.zero
    , Css.fontWeight <| Css.int 600
    , Css.borderBottom3 (Css.px 3) Css.solid values.secondaryColor
    , Css.backgroundColor <| Css.hex "#d6e5ff"
    ]


globalStyles : Html msg
globalStyles =
    GCss.global
        [ GCss.each [ GCss.input, GCss.body, GCss.button ]
            [ Css.fontFamilies values.mainFontFamily ]
        , GCss.body
            [ Css.margin2 (Css.px 8) (Css.px 16)
            ]
        , GCss.each [ GCss.p, GCss.input, GCss.label ]
            [ Css.fontWeight <| Css.int 200
            , Css.margin2 (Css.px 12) Css.zero
            ]
        ]
