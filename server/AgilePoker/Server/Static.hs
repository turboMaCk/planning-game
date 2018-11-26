{-# LANGUAGE OverloadedStrings     #-}

module AgilePoker.Server.Static (staticMiddleware) where

import Network.HTTP.Types (status200)
import Network.Wai (Response, Middleware, responseFile, rawPathInfo)
import Network.Wai.Middleware.Static (staticPolicy, addBase)


indexMiddleware :: Middleware
indexMiddleware application request respond =
    if rawPathInfo request == "/"
    then respond indexRes
    else application request respond

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
