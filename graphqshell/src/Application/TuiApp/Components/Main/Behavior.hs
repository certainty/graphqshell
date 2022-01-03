module Application.TuiApp.Components.Main.Behavior where

import qualified Application.TuiApp.Components.CommandBar.Component as CommandBar
import Application.TuiApp.Components.Main.State (State (State))
import Application.TuiApp.Shared (Action, ComponentName, Event (CmdQuit))
import Brick (AttrName, Padding (Max), hBox, hLimitPercent, joinBorders, padRight, txt, withAttr, withBorderStyle)
import Brick.Themes (Theme, newTheme)
import Brick.Types (Widget)
import Brick.Widgets.Border (border, hBorder)
import Brick.Widgets.Border.Style
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core ((<=>))
import Control.Exception.Safe (MonadThrow)
import qualified Graphics.Vty as V
import Infrastructure.TuiEngine (Continuation (Continue))
import qualified Infrastructure.TuiEngine.Events as TuiEvents
import Infrastructure.TuiEngine.Keymap (KeyMapConfiguration, cmd, fromConfiguration)
import Relude hiding (State, state)

theme :: Theme
theme = newTheme V.defAttr allAttributes
  where
    allAttributes = attributes

attributes :: [(AttrName, V.Attr)]
attributes = [(attrStatusLine, V.defAttr), (attrTopBar, V.defAttr)]

attrStatusLine :: AttrName
attrStatusLine = "main" <> "statusLine"

attrTopBar :: AttrName
attrTopBar = "main" <> "topBar"

rootKeyMapConfig :: KeyMapConfiguration Event
rootKeyMapConfig = cmd 'q' "quit" CmdQuit

-- Component hooks

initial :: (MonadThrow m) => m (Continuation State Action Event)
initial = do
  rootKeyMap <- fromConfiguration rootKeyMapConfig
  pure $ Continue $ State rootKeyMap (CommandBar.makeComponent rootKeyMap) []

update :: (MonadThrow m) => State -> TuiEvents.Event Event -> m (Continuation State Action Event)
update s _ = pure $ Continue s

view :: State -> [Widget ComponentName]
view s = [mainWidget s]

mainWidget :: State -> Widget ComponentName
mainWidget state = withBorderStyle unicodeRounded $ joinBorders $ mainViewPort state

-- TODO: extract into component
topBar :: State -> Widget ComponentName
topBar _state = withAttr attrTopBar $ hBox [padRight Max $ txt "http://example.com"]

mainViewPort :: State -> Widget ComponentName
mainViewPort state =
  border $
    topBar state
      <=> hBorder
      <=> (hLimitPercent 80 $ hCenter $ txt "Middle")
      <=> hBorder

-- <=> statusLine state
-- statusLine :: State -> Widget ComponentName
-- statusLine state = case (activeComponent state) of
--   CommandBarComponent -> CommandBar.view (state ^. stCommandBarRecord . crState) <=> hBorder <=> status (statusLineComponentName CommandBarComponent)
--   name -> status (statusLineComponentName name)
--   where
--     status componentName = withAttr attrStatusLine $ hBox [padRight Max $ padLeft (Pad 1) $ txt componentName]

-- statusLineComponentName :: ComponentName -> Text
-- statusLineComponentName CommandBarComponent = "Menu"
-- statusLineComponentName IntrospectorComponent = "Introspector"
-- statusLineComponentName MainComponent = "Main"
