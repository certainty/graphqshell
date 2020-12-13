module Main where

import           Options.Applicative
import           Relude
import qualified Shell.Application             as Application
import qualified Data.ByteString               as ByteString
import           Shell.Configuration
import           System.Directory

data Options = Options
  { verbose :: Bool,
    configPath :: FilePath
  }
  deriving (Eq, Show)

main :: IO ()
main = do
  userHomePath <- getHomeDirectory
  parsedOpts   <- execParser (opts userHomePath)
  config       <- loadConfiguration (configPath parsedOpts)
  void $ Application.run config
 where
  opts homePath = info
    ((options homePath) <**> helper)
    (fullDesc <> header "GraphQL TUI that is fast, fun and functional")

loadConfiguration :: FilePath -> IO ApplicationConfig
loadConfiguration path = do
  canonicalPath <- makeAbsolute path
  (ByteString.readFile canonicalPath) >>= (parseConfiguration canonicalPath)

options :: FilePath -> Parser Options
options homePath =
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
          <> value (homePath <> "/.graphqshell/config.yaml")
          )
