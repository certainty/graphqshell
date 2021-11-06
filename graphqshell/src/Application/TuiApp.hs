module Application.TuiApp where

import Application.TuiApp.Components.Main
import Application.TuiApp.Configuration
import Application.TuiApp.IO (ioHandler)
import qualified Data.ByteString as ByteString
import qualified Infrastructure.TuiEngine as Tui
import Options.Applicative
import Relude
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

data Options = Options
  { verbose :: Bool,
    configPath :: FilePath
  }
  deriving (Eq, Show)

main :: IO ()
main = do
  defaultConfigPath <- defaultConfigurationFilePath
  parsedOpts <- execParser (opts defaultConfigPath)
  config <- loadConfiguration (configPath parsedOpts)
  void $ Tui.run engine (engineConfig config)
  where
    opts homePath =
      info
        ((options homePath) <**> helper)
        (fullDesc <> header "GraphQL TUI that is fast, fun and functional")

engine :: Tui.Engine
engine = Tui.Engine ioHandler Main.component

loadConfiguration :: FilePath -> IO ApplicationConfig
loadConfiguration path = do
  absolutePath <- Directory.makeAbsolute path
  ByteString.readFile absolutePath >>= (parseConfiguration absolutePath)

defaultConfigurationFilePath :: IO FilePath
defaultConfigurationFilePath = do
  configDirectory <- Directory.getXdgDirectory Directory.XdgConfig "graphqshell"
  pure $ configDirectory FilePath.</> "config.yaml"

options :: FilePath -> Parser Options
options defaultConfigPath =
  Options
    <$> switch (long "verbose" <> short 'v' <> help "Print information during startup")
    <*> strOption
      ( long "config"
          <> short 'c'
          <> help "The path to the configuration file"
          <> metavar "CONFIG_FILE"
          <> showDefault
          <> value defaultConfigPath
      )
