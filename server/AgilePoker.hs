module Main where

import qualified AgilePoker.Api as Api
import qualified Network.Wai.Handler.Warp as Warp


main :: IO ()
main = do
  state <- Api.initState
  Warp.runEnv 3000 $ Api.app state
