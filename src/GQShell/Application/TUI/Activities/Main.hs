{-# LANGUAGE TemplateHaskell #-}

module GQShell.Application.TUI.Activities.Main where

import Brick (fill, hLimit, hLimitPercent, joinBorders, padLeft, padRight, txt, vBox, withAttr)
import Brick.Types
import Brick.Widgets.Core (hBox, vLimit)
import Control.Exception.Safe (MonadThrow)
import GQShell.Application.Config.Types (EndpointConfig)
import GQShell.Application.TUI.Activities.Main.EndpointMenu
import qualified GQShell.Application.TUI.Activities.Main.EndpointMenu as EndpointMenu
import GQShell.Application.TUI.Activities.Main.StatusBar (StatusBarModel)
import qualified GQShell.Application.TUI.Activities.Main.StatusBar as StatusBar
import GQShell.Application.TUI.Activities.Main.Tabs (TabModel)
import qualified GQShell.Application.TUI.Activities.Main.Tabs as Tabs
import GQShell.Application.TUI.Shared
import GQShell.Application.TUI.Style (keyMapHelpDesc, keyMapHelpKey)
import Hubble.Hubbles.Log (LogModel)
import qualified Hubble.Hubbles.Log as Logs
import Hubble.Internal.Types
import Hubble.KeyMap (Binding, Help, bindingHelp, helpDescription, helpKey, matches)
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
  tabTile <- makeTile <$> Tabs.newModel
  pure $ mkModel (MainState km (makeTile menuModel) logTile tabTile statusBarTile Disconnected) [] mainUpdate mainView
  where
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
  | isVisible (_msLogs mainState) =
    vBox
      [ viewTile' (mainState ^. msStatusBar),
        contentView mainState,
        viewTile' (_msLogs mainState),
        contextHelpView mainState
      ]
  | otherwise =
    vBox
      [ viewTile' (mainState ^. msStatusBar),
        contentView mainState,
        contextHelpView mainState
      ]

focusedKeyBindings :: MainState -> [Binding]
focusedKeyBindings mainState
  | hasFocus (mainState ^. msSections) = Tabs.keyBindings (mainState ^. msSections . tData)
  | hasFocus (mainState ^. msEndpointMenu) = EndpointMenu.keyBindings (mainState ^. msEndpointMenu . tData)
  | otherwise = []

globalKeyBindings :: MainState -> [Binding]
globalKeyBindings s = fmap (\f -> f keyMap) [_pkQuit, _pkToggleLogs, _pkToggleEndpointMenu]
  where
    keyMap = s ^. msKeyMap

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

contextHelpView :: MainState -> Widget ()
contextHelpView mainState =
  vLimit 3 $
    vBox
      [ hBox [fill ' '],
        hBox
          [ padLeft (Pad 1) globalContextHelp,
            fill ' ',
            focusedContextHelp
          ],
        fill ' '
      ]
  where
    globalContextHelp = padLeft (Pad 1) $ contextualHelp (globalKeyBindings mainState)
    focusedContextHelp = padRight (Pad 1) $ contextualHelp (focusedKeyBindings mainState)

contextualHelp :: [Binding] -> Widget ()
contextualHelp bindings = hBox $ contextHelpEntry <$> helpInfos
  where
    helpInfos = mapMaybe (^. bindingHelp) bindings

contextHelpEntry :: Help -> Widget ()
contextHelpEntry help = padRight (Pad 2) $ hBox [bindingKey, bindingDescription]
  where
    bindingKey = withAttr keyMapHelpKey $ padRight (Pad 1) $ txt $ help ^. helpKey
    bindingDescription = withAttr keyMapHelpDesc $ txt $ help ^. helpDescription
