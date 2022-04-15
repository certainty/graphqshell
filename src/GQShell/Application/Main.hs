{-# LANGUAGE OverloadedStrings #-}

module GQShell.Application.Main (main) where

import qualified Data.Text.IO as T
import GQShell.Application.Config (ApplicationConfig, appConfigEndpoints)
import GQShell.Application.Config.Types (appConfigDefaultEndpoint, appConfigTickRate)
import qualified GQShell.Application.TUI.Activities.Main as Main
import GQShell.Application.TUI.IOHandler (commandHandler)
import GQShell.Application.TUI.Shared
import qualified GQShell.Application.TUI.Style as Style
import Hubble.Program
import Lens.Micro.Platform ((^.))
import Relude (fromMaybe)
import System.Exit
import Prelude

main :: ApplicationConfig -> IO ()
main cfg = do
  let opts = ProgramOptions (Millis tickRate) Style.attrMap
  programKeys <- mkProgramKeys
  let _defaultEndpoint = cfg ^. appConfigDefaultEndpoint
  mainModel <- Main.newModel (cfg ^. appConfigEndpoints) programKeys
  result <- startProgram mainModel commandHandler opts
  case result of
    (Left message) -> T.putStrLn message >> exitWith (ExitFailure 1)
    Right _ -> pure ()
  where
    tickRate = fromMaybe 100 (cfg ^. appConfigTickRate)
