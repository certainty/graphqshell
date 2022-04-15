{-# LANGUAGE TemplateHaskell #-}

module GQShell.Application.TUI.Activities.Main where

import Brick (fill, hLimit, hLimitPercent, joinBorders, padLeft, padRight, vBox)
import Brick.Types
import Brick.Widgets.Core (hBox, vLimit)
import Control.Exception.Safe (MonadThrow)
import GQShell.Application.Config (EndpointConfig)
import GQShell.Application.TUI.Activities.Main.EndpointMenu
import qualified GQShell.Application.TUI.Activities.Main.EndpointMenu as EndpointMenu
import GQShell.Application.TUI.Activities.Main.StatusBar (StatusBarModel)
import qualified GQShell.Application.TUI.Activities.Main.StatusBar as StatusBar
import GQShell.Application.TUI.Activities.Main.Tabs (TabModel)
import qualified GQShell.Application.TUI.Activities.Main.Tabs as Tabs
import GQShell.Application.TUI.Shared
import Hubble.Hubbles.Log (LogModel)
import qualified Hubble.Hubbles.Log as Logs
import Hubble.Internal.Types
import Hubble.KeyMap (matches)
import Hubble.Program (cont, exit, logError)
import Lens.Micro.Platform (makeLenses, (^.))
import Relude

data MainState = MainState
  { _msKeyMap :: ProgramKeys,
    _msEndpointMenu :: Tile EndpointMenuModel,
    _msLogs :: Tile (LogModel AppCommand AppMessage),
    -- | The detail view contains multiple tabs. The detail view is a focusable tile
    _msSections :: Tile TabModel,
    _msStatusBar :: Tile StatusBarModel,
    _msSelectedEndpoint :: EndpointState
  }

makeLenses ''MainState

newModel :: (MonadThrow m) => [EndpointConfig] -> ProgramKeys -> m (Model' MainState)
newModel endpoints km = do
  menuModel <- EndpointMenu.newModel Focus endpoints
  pure $ mkModel (MainState km (makeTile menuModel) logTile tabTile statusBarTile Disconnected) [] mainUpdate mainView
  where
    tabTile = makeTile Tabs.newModel
    logTile = setInvisible $ makeTile (Logs.newModel 10 Warning)
    statusBarTile = makeTile (StatusBar.newModel Disconnected)

mainUpdate :: MainState -> Message' -> UpdateM (MainState, [Command'])
mainUpdate mainState (CommandFailed commandError) = logError ("Command failed: " <> show commandError) >> cont mainState
mainUpdate mainState keyMsg@(KeyMsg k)
  | _pkQuit km `matches` k = exit mainState
  | _pkToggleLogs km `matches` k = cont mainState {_msLogs = toggleVisibility (_msLogs mainState)}
  | _pkToggleEndpointMenu km `matches` k = cont (toggleMenuAndDetails mainState)
  | otherwise = relayMessage mainState keyMsg
  where
    km = mainState ^. msKeyMap
mainUpdate mainState msg@(AppMsg (EndpointChange endpointState@(Connected _ _))) = do
  (mainState', commands') <- relayMessage (toggleMenuAndDetails mainState) msg
  pure (mainState' {_msSelectedEndpoint = endpointState}, commands')
mainUpdate mainState msg = relayMessage mainState msg

relayMessage :: MainState -> Message' -> UpdateM (MainState, [Command'])
relayMessage mainState msg = do
  (endpointMenu', endpointCommands) <- updateTile (_msEndpointMenu mainState) msg
  (sections', sectionCommands) <- updateTile (_msSections mainState) msg
  (statusBar', statusBarCommands) <- updateTile (_msStatusBar mainState) msg
  (logTile', logCommands) <- updateTile' (_msLogs mainState) msg
  pure
    ( mainState
        { _msEndpointMenu = endpointMenu',
          _msSections = sections',
          _msLogs = logTile',
          _msStatusBar = statusBar'
        },
      endpointCommands <> sectionCommands <> statusBarCommands <> logCommands
    )

-- toggle the menu and manage the focus of the detail
toggleMenuAndDetails :: MainState -> MainState
toggleMenuAndDetails mainState =
  let mainState' = toggleFocusL (msEndpointMenu . focusL) mainState
      menuTile = mainState' ^. msEndpointMenu
      sections = mainState' ^. msSections
   in mainState'
        { _msEndpointMenu = toggleFocus (toggleVisibility menuTile),
          _msSections = toggleFocus sections
        }

mainView :: MainState -> Widget ()
mainView mainState
  | isVisible (_msLogs mainState) = vBox [vLimit 3 $ fill ' ', contentView mainState, viewTile' (_msLogs mainState), viewTile' (mainState ^. msStatusBar)]
  | otherwise =
    vBox
      [ vLimit 3 $ fill ' ',
        contentView mainState,
        viewTile' (mainState ^. msStatusBar)
      ]

contentView :: MainState -> Widget ()
contentView mainState = hBox [menuWidget, tabWidget]
  where
    menuWidget = if isVisible (mainState ^. msEndpointMenu) then endpointListView mainState else hLimit 2 (fill ' ')
    tabWidget = joinBorders $ viewTile' (_msSections mainState)

endpointListView :: MainState -> Widget ()
endpointListView mainState =
  hLimitPercent 10 $
    padLeft (Pad 2) $
      padRight (Pad 2) $
        viewTile' (_msEndpointMenu mainState)
