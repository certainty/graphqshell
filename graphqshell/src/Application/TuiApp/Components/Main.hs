module Application.TuiApp.Components.Main (component, theme) where

import Application.TuiApp.Shared
import Brick (AttrMap, AttrName, Padding (Max), hBox, joinBorders, padRight, txt, withAttr, withBorderStyle)
import Brick.Themes (Theme, newTheme)
import Brick.Types (Widget)
import Brick.Widgets.Border (border, hBorder)
import Brick.Widgets.Border.Style
import Brick.Widgets.Core ((<=>))
import Data.Yaml.Aeson (defaultStringStyle)
import qualified Graphics.Vty as V
import qualified Infrastructure.TuiEngine as Tui
import Infrastructure.TuiEngine.Components
import Relude hiding (State)

component :: (Component State Action Event ComponentName)
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

initial :: Continuation State Action Event
initial = Continue s
  where
    s :: State
    s = State

update :: State -> Tui.Event Event -> Continuation State Action Event
update s _ = Continue s

view :: State -> [Widget ComponentName]
view state = [mainWidget state]

mainWidget :: State -> Widget ComponentName
mainWidget state = withBorderStyle unicodeRounded $ joinBorders $ mainViewPort state

-- TODO: extract into component
topBar :: State -> Widget ComponentName
topBar state = withAttr attrTopBar $ hBox [padRight Max $ txt "http://example.com"]

mainViewPort :: State -> Widget ComponentName
mainViewPort state =
  border $
    topBar state
      <=> hBorder
      -- <=> Intro.view (state ^. stIntrospectorRecord . crState)
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
