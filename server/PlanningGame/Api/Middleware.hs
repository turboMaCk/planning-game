{-# LANGUAGE OverloadedStrings #-}

module PlanningGame.Api.Middleware (static) where

import           Data.Maybe                    (fromMaybe)
import           Network.HTTP.Types            (status200)
import           Network.Wai                   (Middleware, Response,
                                                requestHeaders,
                                                responseLBS)
import           Network.Wai.Middleware.Static (addBase, staticPolicy)
import           Network.Wai.Parse             (parseHttpAccept)
import           Text.Blaze                    (AttributeValue)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Blaze.Html5              (Html, (!))

import qualified Text.Blaze                    as Blaze
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as Attrs


index :: Middleware
index application request respond
  | respondWithIndex =
    respond indexRes
  | otherwise =
    application request respond

  where
    respondWithIndex :: Bool
    respondWithIndex
      | "Upgrade" `lookup` (requestHeaders request) == Just "websocket" = False
      | otherwise =
        fromMaybe False $ (elem "text/html" . parseHttpAccept) <$>
        "Accept" `lookup ` (requestHeaders request)

    indexRes :: Response
    indexRes =
      responseLBS status200 headers $ renderHtml indexView

        where
            headers =
              [ ( "Content-Type", "text/html" )
              , ( "Cache-Control", "public, max-age=86400" )
              ]


public :: Middleware
public =
  staticPolicy $ addBase "public"


static :: Middleware
static =
  public . index


indexView :: Html
indexView =
  Html.docTypeHtml ! Attrs.lang "en_EN" $ do
    Html.head $ do
      Html.title "Planning Game"

      Html.meta
        ! Attrs.httpEquiv "x-ua-compatible"
        ! Attrs.content "ie=edge"

      Html.meta
        ! Attrs.charset "UTF-8"

      Html.meta
        ! Attrs.name "description"
        ! Attrs.content description

      Html.meta
        ! Attrs.name "keywords"
        ! Attrs.content "planning,game,voting,poker,agile,scrum,free"

      Html.meta
        ! Blaze.customAttribute "property" "og:title"
        ! Attrs.content title

      Html.meta
        ! Blaze.customAttribute "property" "og:description"
        ! Attrs.content description

      Html.meta
        ! Blaze.customAttribute "property" "og:image"
        ! Attrs.type_ "image/png"
        ! Attrs.href "/favicon.png"

      Html.link
        ! Attrs.rel "icon"
        ! Attrs.href "/favicon.png"
        ! Attrs.type_ "image/png"

      Html.link
        ! Attrs.href "https://fonts.googleapis.com/css?family=Roboto+Slab:300,400,700"
        ! Attrs.rel "stylesheet"

    Html.body $ do
      Html.script ""
        ! Attrs.src "/app.js"

      Html.script ""
        ! Attrs.src "/init.js"

  where
    title, description :: AttributeValue
    description = "Planning tool for remote teams."
    title       = "Planning Game"
