module Main where
import Relude
import Shell.Main (runShell)

main :: IO ()
main = runShell "https://graphql-weather-api.herokuapp.com/"
