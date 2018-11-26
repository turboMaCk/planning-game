module Main where

import qualified AgilePoker.Server as Server

main :: IO ()
main = Server.run 3000
