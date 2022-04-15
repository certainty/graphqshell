{-# LANGUAGE TemplateHaskell #-}

module GQShell.Application.TUI.Activities.Main.StatusBar where

import Brick (Padding (Pad), Widget, fill, hBox, padBottom, padLeft, padRight, vLimit, withAttr)
import Brick.Widgets.Core (txt)
import GQShell.Application.Config.Types (endpointURL)
import GQShell.Application.TUI.Shared (AppCommand, AppMessage (EndpointChange), Command', EndpointState (Connected, Connecting, Disconnected), Message', Model', Tile, makeTile, setInvisible, setVisible, updateTile, viewTile)
import qualified GQShell.Application.TUI.Style as Style
import Hubble.Hubbles.Spinner (SpinnerModel)
import qualified Hubble.Hubbles.Spinner as Spinner
import Hubble.Program (Message (AppMsg), UpdateM, cont, logInfo, mkModel)
import Lens.Micro.Platform
import Relude hiding (state)
import Text.URI (render)

type SpinnerModel' = SpinnerModel AppCommand AppMessage

data StatusBarState = StatusBarState
  { _sbsEndpointState :: EndpointState,
    _sbsSpinner :: Tile SpinnerModel'
  }

makeLenses ''StatusBarState

type StatusBarModel = Model' StatusBarState

newModel :: EndpointState -> StatusBarModel
newModel currentEndpointState = mkModel (StatusBarState currentEndpointState spinnerTile) mempty statusBarUpdate statusBarView
  where
    spinnerTile = setInvisible $ makeTile $ Spinner.newModel glyphs
    glyphs = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]

statusBarUpdate :: StatusBarState -> Message' -> UpdateM (StatusBarState, [Command'])
statusBarUpdate state (AppMsg (EndpointChange newEndpointState)) = logInfo "Status update" >> cont newState
  where
    newState = (state & sbsEndpointState .~ newEndpointState) & sbsSpinner .~ updatedSpinner
    updatedSpinner = case newEndpointState of
      (Connecting _) -> setVisible $ state ^. sbsSpinner
      _ -> setInvisible $ state ^. sbsSpinner
statusBarUpdate state msg = do
  (spinner', _) <- updateTile (state ^. sbsSpinner) msg
  pure (state & sbsSpinner .~ spinner', [])

statusBarView :: StatusBarState -> Widget ()
statusBarView state =
  padBottom (Pad 1) $
    withAttr Style.statusBar $
      vLimit 1 $ hBox $ catMaybes [Just endpointState, Just statusGap]
  where
    statusCaption caption = withAttr Style.statusCaption $ padLeft (Pad 1) $ padRight (Pad 1) $ hBox $ catMaybes [viewTile (state ^. sbsSpinner), Just $ txt caption]
    statusGap = fill ' '
    endpointURLV endpoint = padLeft (Pad 2) $ txt $ render (endpoint ^. endpointURL)
    endpointState = case state ^. sbsEndpointState of
      Disconnected -> statusCaption "Disconnected "
      Connecting endpoint -> hBox [statusCaption "Connecting", endpointURLV endpoint]
      Connected endpoint _ -> hBox [statusCaption "Connected", endpointURLV endpoint]
