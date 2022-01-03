module Application.TuiApp where

import qualified Application.TuiApp.Components.Main.Behavior as Main
import qualified Application.TuiApp.Components.Main.Component as Main
import qualified Application.TuiApp.Components.Main.State as Main
import Application.TuiApp.Configuration
import Application.TuiApp.IO (ioHandler)
import Application.TuiApp.Shared
import Control.Exception.Safe (MonadThrow)
import qualified Data.ByteString as ByteString
import Data.Default (Default (def))
import Infrastructure.TuiEngine (EngineConfiguration (_confTickRate))
import qualified Infrastructure.TuiEngine as Tui
import Options.Applicative
import Relude hiding (State)
import Shell.Configuration ()
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
  void $ Tui.run (engineConfig config)
  where
    opts homePath =
      info
        ((options homePath) <**> helper)
        (fullDesc <> header "GraphQL TUI that is fast, fun and functional")

engineConfig :: (MonadThrow m) => ApplicationConfig -> Tui.EngineConfiguration Main.State Action Event ComponentName m
engineConfig _ =
  Tui.Configuration
    { _confIOHandler = Just ioHandler,
      _confMainComponent = Main.makeComponent,
      _confTheme = Main.theme,
      _confTickRate = def
    }

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
