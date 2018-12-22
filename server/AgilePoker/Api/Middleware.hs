{-# LANGUAGE OverloadedStrings #-}

module AgilePoker.Api.Middleware (staticMiddleware) where

import           Data.Maybe                    (fromMaybe)
import           Network.HTTP.Types            (status200)
import           Network.Wai                   (Middleware, Response,
                                                rawPathInfo, requestHeaders,
                                                responseFile)
import           Network.Wai.Middleware.Static (addBase, staticPolicy)
import           Network.Wai.Parse             (parseHttpAccept)


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
    indexRes = responseFile status200 headers fileName Nothing
        where
            fileName = "public/index.html"
            headers = [ ( "Content-Type", "text/html" )
                      , ( "Cache-Control", "public, max-age=86400" )
                      ]


publicMiddleware :: Middleware
publicMiddleware =
  staticPolicy $ addBase "public"


staticMiddleware :: Middleware
staticMiddleware =
  publicMiddleware . indexMiddleware
