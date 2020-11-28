module Main where

import Options.Applicative
import Relude
import Shell.Application (runShell)
import System.Environment

data Options = Options
  { verbose :: Bool,
    apiURL :: Text
  }
  deriving (Eq, Show)

tickRate :: Int
tickRate = (1 * 1000000)

main :: IO ()
main = do
  parsedOpts <- execParser opts
  apiURI <- lookupEnv "GRAPHQL_API"
  void $ runShell (fromMaybe (toString (apiURL parsedOpts)) apiURI) tickRate
  where
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> header "GraphQL TUI that is fast, fun and functional"
        )

options :: Parser Options
options =
  Options
    <$> switch
      ( long "verbose"
          <> short 'v'
          <> help "Print information during startup"
      )
    <*> strOption
      ( long "api"
          <> short 'a'
          <> help "The GRAPHQL API to connect to initially"
          <> metavar "GRAPHQL_API"
          <> showDefault
          <> value "https://graphql-weather-api.herokuapp.com/"
      )
