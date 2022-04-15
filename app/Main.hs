module Main (main) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as B
import GQShell.Application.Config
import qualified GQShell.Application.Main as Application
import Options.Applicative
import Relude
import System.Directory (doesFileExist)
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
  configExists <- doesFileExist (configPath parsedOpts)
  if not configExists
    then do
      putStrLn $ "Configuration file does not exist: " <> configPath parsedOpts <> ". Please create one. See help for more info."
      putStrLn "A default configuration looks like this: "
      B.putStrLn "TBD"
      exitFailure
    else do
      config <- loadConfiguration (configPath parsedOpts)
      void $ Application.main config
  where
    opts homePath =
      info
        (options homePath <**> helper)
        (fullDesc <> header "GraphQL TUI that is fast, fun and functional")

loadConfiguration :: FilePath -> IO ApplicationConfig
loadConfiguration path = do
  absolutePath <- Directory.makeAbsolute path
  ByteString.readFile absolutePath >>= parseConfiguration

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
