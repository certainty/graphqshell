module Main where

import Relude
import Shell.Main (runShell)
import System.Environment

main :: IO ()
main = do
  apiURI <- lookupEnv "GRAPHQL_API"
  runShell (fromMaybe "https://graphql-weather-api.herokuapp.com/" apiURI)
