{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Api.Middleware (staticMiddleware) where

import           Data.Maybe                    (fromMaybe)
import           Network.HTTP.Types            (status200)
import           Network.Wai                   (Middleware, Response,
                                                rawPathInfo, requestHeaders,
                                                responseLBS)
import           Network.Wai.Middleware.Static (addBase, staticPolicy)
import           Network.Wai.Parse             (parseHttpAccept)

import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Blaze.Html5              as Html
import           Text.Blaze.Html5.Attributes   as Attrs


indexMiddleware :: Middleware
indexMiddleware application request respond =
  if fromMaybe False $
     (elem "text/html" . parseHttpAccept) <$>
     lookup "Accept" (requestHeaders request)
  then
    respond indexRes
  else
    application request respond

  where
    indexRes :: Response
    indexRes =
      responseLBS status200 headers $ renderHtml index
        where
            headers = [ ( "Content-Type", "text/html" )
                      , ( "Cache-Control", "public, max-age=86400" )
                      ]


publicMiddleware :: Middleware
publicMiddleware =
  staticPolicy $ addBase "public"


staticMiddleware :: Middleware
staticMiddleware =
  publicMiddleware . indexMiddleware


index :: Html
index =
  docTypeHtml ! lang "en_EN" $ do
    Html.head $ do
      Html.title "Agile Poker"
      Html.meta
        ! Attrs.charset "UTF-8"
      Html.link
        ! Attrs.href "https://fonts.googleapis.com/css?family=Roboto+Slab:300,400,700"
        ! Attrs.rel "stylesheet"

    Html.body $ do
      Html.script ""
        ! Attrs.src "/app.js"
      Html.script ""
        ! Attrs.src "/init.js"
