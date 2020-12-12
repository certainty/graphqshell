module Main where

import           Options.Applicative
import           Relude
import qualified Shell.Application             as Application
import qualified Data.ByteString               as ByteString
import           Shell.Configuration

data Options = Options
  { verbose :: Bool,
    configPath :: FilePath
  }
  deriving (Eq, Show)

tickRate :: Int
tickRate = 1 * 1000000

main :: IO ()
main = do
  parsedOpts <- execParser opts
  config     <- loadConfiguration (configPath parsedOpts)
  void $ Application.run config tickRate
 where
  opts = info
    (options <**> helper)
    (fullDesc <> header "GraphQL TUI that is fast, fun and functional")

loadConfiguration :: FilePath -> IO ApplicationConfig
loadConfiguration path = (ByteString.readFile path) >>= parseConfiguration

options :: Parser Options
options =
  Options
    <$> switch
          (long "verbose" <> short 'v' <> help
            "Print information during startup"
          )
    <*> strOption
          (  long "config"
          <> short 'c'
          <> help "The path to the configuration file"
          <> metavar "CONFIG_FILE"
          <> showDefault
          <> value "~/.graphqshell/config.yaml"
          )
