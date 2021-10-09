module Main
  ( main
  )
where

import           Options.Applicative
import           Relude
import qualified Shell.Application             as Application
import qualified Data.ByteString               as ByteString
import           Shell.Configuration
import qualified System.Directory              as Directory
import qualified System.FilePath               as FilePath

data Options = Options
  { verbose    :: Bool
  , configPath :: FilePath
  }
  deriving (Eq, Show)

main :: IO ()
main = do
  defaultConfigPath <- defaultConfigurationFilePath
  parsedOpts        <- execParser (opts defaultConfigPath)
  config            <- loadConfiguration (configPath parsedOpts)
  void $ Application.run config
 where
  opts homePath = info
    (options homePath <**> helper)
    (fullDesc <> header "GraphQL TUI that is fast, fun and functional")

loadConfiguration :: FilePath -> IO ApplicationConfig
loadConfiguration path = do
  absolutePath <- Directory.makeAbsolute path
  ByteString.readFile absolutePath >>= parseConfiguration absolutePath

defaultConfigurationFilePath :: IO FilePath
defaultConfigurationFilePath = do
  configDirectory <- Directory.getXdgDirectory Directory.XdgConfig "graphqshell"
  pure $ configDirectory FilePath.</> "config.yaml"

options :: FilePath -> Parser Options
options defaultConfigPath =
  Options
    <$> switch (long "verbose" <> short 'v' <> help "Print information during startup")
    <*> strOption
          (  long "config"
          <> short 'c'
          <> help "The path to the configuration file"
          <> metavar "CONFIG_FILE"
          <> showDefault
          <> value defaultConfigPath
          )
