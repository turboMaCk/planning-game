module Page.Table.Players exposing (PlayerVote(..), view)

import Css
import Data exposing (Player, Vote(..))
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Maybe.Extra as Maybe
import Theme


type PlayerVote
    = Hidden
    | Unknown
    | Voted Vote


fromPlayerVote : PlayerVote -> Vote
fromPlayerVote v =
    case v of
        Hidden ->
            InfinityPoints

        Unknown ->
            InfinityPoints

        Voted vote ->
            vote


type alias Config msg =
    { isMe : Player -> Bool
    , isDealer : Player -> Bool
    , toVote : Player -> PlayerVote
    , select : Player -> msg
    , deselect : msg
    , kickPlayer : Player -> msg
    }


voteIndicator : PlayerVote -> Html msg
voteIndicator playerVote =
    let
        show txt =
            Html.styled Html.span
                [ Theme.pill
                , Css.backgroundColor Theme.values.primaryColor
                , Css.borderRadius <| Css.pct 100
                ]
                [ Attrs.title <| "player voted: " ++ txt ]
                [ Html.text txt ]
    in
    case playerVote of
        Hidden ->
            Html.text ""

        Unknown ->
            show "?"

        Voted vote ->
            show <| String.fromInt <| Data.voteToInt vote


dealerIndicator : Bool -> Html msg
dealerIndicator isDealer =
    if isDealer then
        Html.styled Html.span
            [ Theme.pill
            , Css.backgroundColor Theme.values.secondaryColor
            , Css.marginLeft Css.zero
            ]
            [ Attrs.title "Dealer" ]
            [ Html.text "D" ]

    else
        Html.text ""


onlineIndicator : Bool -> Html msg
onlineIndicator isActive =
    let
        size =
            Css.px 8

        text =
            if isActive then
                "online"

            else
                "offlie"
    in
    Html.styled Html.span
        [ Css.display Css.inlineBlock
        , Css.width size
        , Css.height size
        , Css.borderRadius <| Css.pct 100
        , Css.overflow <| Css.hidden
        , Css.textIndent <| Css.px -999
        , Css.margin4 Css.zero (Css.px 6) (Css.px 2) Css.zero
        , Css.backgroundColor <|
            if isActive then
                Theme.values.greenColor

            else
                Theme.values.primaryColor
        ]
        [ Attrs.title text ]
        [ Html.text text
        ]


concatWhen : Bool -> List a -> List a -> List a
concatWhen b xs =
    if b then
        List.append xs

    else
        identity


viewPlayer : Config msg -> Bool -> Maybe Int -> Maybe Vote -> Player -> Html msg
viewPlayer { isMe, toVote, select, deselect, kickPlayer, isDealer } amIDealer selectedId highlightedVote player =
    let
        showKick =
            amIDealer && not (isMe player)

        isSelected =
            Just player.id == selectedId
    in
    Html.styled Html.li
        ([ Css.listStyle Css.none
         , Css.marginBottom <| Css.px 6
         , Css.fontWeight <| Css.int 200
         , Css.overflow Css.hidden
         , Css.textOverflow Css.ellipsis
         ]
            |> concatWhen isSelected
                [ Css.padding3 (Css.px 2) (Css.px 6) <| Css.px 6
                , Css.backgroundColor Theme.values.lightBackground
                , Css.borderRadius <| Css.px 4
                , Css.marginLeft <| Css.px -6
                , Css.marginRight <| Css.px -6
                , Css.marginTop <| Css.px -2
                ]
        )
        []
        [ onlineIndicator player.isConnected
        , dealerIndicator <| isDealer player
        , Html.styled Html.span
            ([ Css.cursor Css.pointer
             , Css.hover [ Css.textDecoration Css.underline ]
             ]
                |> concatWhen (Just (fromPlayerVote (toVote player)) == highlightedVote)
                    [ Css.fontWeight <| Css.bold ]
                |> concatWhen (isMe player) [ Css.textDecoration Css.underline ]
            )
            [ Events.onClick <|
                if isSelected then
                    deselect

                else
                    select player
            ]
            [ Html.text player.name ]
        , voteIndicator <| toVote player
        , if isSelected then
            Html.styled Html.div
                []
                []
                [ if showKick then
                    Html.styled Html.button
                        [ Theme.secondaryBtn
                        , Css.fontSize <| Css.px 11
                        , Css.marginTop <| Css.px 4
                        ]
                        [ Events.onClick <| kickPlayer player ]
                        [ Html.text "kick out" ]

                  else
                    Html.styled Html.small
                        [ Css.fontSize <| Css.px 11 ]
                        []
                        [ Html.text <|
                            if amIDealer then
                                "Dealer can't be kicked out"

                            else
                                "Only delaer can kick our players"
                        ]
                ]

          else
            Html.text ""
        ]


view : Config msg -> Bool -> Maybe Int -> Maybe Vote -> Dict Int Player -> Html msg
view config amIDealer selectedId highlightedVote players =
    Html.div []
        [ Html.styled Html.ul
            [ Css.listStyle Css.none
            , Css.margin Css.zero
            , Css.padding Css.zero
            ]
            []
          <|
            (List.map (viewPlayer config amIDealer selectedId highlightedVote) <|
                Dict.values players
            )
        ]
