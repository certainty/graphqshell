{-# LANGUAGE TemplateHaskell #-}

module Application.TuiApp.Components.Main (component, theme, State) where

import qualified Application.TuiApp.Components.CommandBar as CommandBar
import Application.TuiApp.Shared
import Brick (AttrMap, AttrName, Padding (Max), hBox, hLimitPercent, joinBorders, padRight, txt, withAttr, withBorderStyle)
import Brick.Themes (Theme, newTheme)
import Brick.Types (Widget)
import Brick.Widgets.Border (border, hBorder)
import Brick.Widgets.Border.Style
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core ((<=>))
import Control.Exception.Safe (MonadThrow)
import qualified Graphics.Vty as V
import qualified Infrastructure.TuiEngine as Tui
import Infrastructure.TuiEngine.Components
import Infrastructure.TuiEngine.Keymap
import Lens.Micro.Platform (makeLenses, (^.))
import Relude hiding (State, state)

data ComponentRecord s = ComponentRecord
  { _crName :: ComponentName,
    _crState :: s,
    _crKeyMap :: Maybe (KeyMap Event)
  }

makeLenses ''ComponentRecord

-- | The application state holds global data and component specific data.
-- The application will delegate updates to the currently active component automatically.
data State = State
  { _stKeyMap :: KeyMap Event,
    _commandBar :: CommandBar.ComponentType IO,
    _stComponentStack :: ![ComponentName]
  }

makeLenses ''State

component :: (MonadThrow m) => (Component State Action Event ComponentName m)
component =
  Component
    { _componentName = Main,
      _componentInitial = initial,
      _componentUpdate = update,
      _componentView = Just view
    }

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
  pure $ Continue $ State rootKeyMap (CommandBar.component rootKeyMap) []

update :: (MonadThrow m) => State -> Tui.Event Event -> m (Continuation State Action Event)
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
