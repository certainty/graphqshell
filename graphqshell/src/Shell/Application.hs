{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Shell.Application (runShell) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified GraphQL.API as API
import GraphQL.Introspection.Schema (Schema)
import qualified Graphics.Vty as V
import Lens.Micro (Lens', (&), (.~), (^.))
import Lens.Micro.TH
import Relude hiding (state)
import qualified Shell.Components.Introspection as Intro
import Text.URI (renderStr)

data Name = SchemaView deriving (Eq, Ord, Show)

-- Welcome the app-state
data ApplicationState = ApplicationState
  { _schema :: Schema,
    _apiSettings :: API.ApiSettings,
    _introspectorState :: Intro.State
  }
  deriving (Eq, Show)

makeLenses ''ApplicationState

-- Application Events
data ApplicationEvent
  = IntrospectorEvent Intro.Event
  | Tick
  deriving (Eq, Ord, Show)

data Settings = Settings
  { -- The URL of the API to connect to on application start up
    apiUrl :: Text
  }
  deriving (Eq, Show)

runShell :: String -> IO ()
runShell url = do
  apiSettings' <- API.mkApiSettings (toText url)
  schema' <- API.runApiIO apiSettings' API.introspect
  void $ defaultMain makeApplication (ApplicationState schema' apiSettings' (Intro.mkState schema'))

makeApplication :: App ApplicationState ApplicationEvent ()
makeApplication =
  App
    { appDraw = draw,
      appChooseCursor = neverShowCursor,
      appHandleEvent = update,
      appAttrMap = const $ attrMap V.defAttr [],
      appStartEvent = pure
    }

update :: ApplicationState -> BrickEvent n ApplicationEvent -> EventM n (Next ApplicationState)
update s (AppEvent (IntrospectorEvent event)) = updateComponent s introspectorState Intro.update (AppEvent event)
update s (VtyEvent evt) = updateComponent s introspectorState Intro.update (VtyEvent evt)
update s _ = continue s

updateComponent :: a -> Lens' a b -> (b -> e -> EventM n (Next b)) -> e -> EventM n (Next a)
updateComponent state target handler event = do
  newVal <- handler (state ^. target) event
  pure $ (\newState -> state & target .~ newState) <$> newVal

draw :: ApplicationState -> [Widget ()]
draw st = [ui uriStr]
  where
    uriStr = renderStr . API.apiURI . _apiSettings $ st

topBar :: String -> Widget n
topBar url = hBox [padRight Max $ str $ "connected to " ++ url]

statusLine :: Widget ()
statusLine = hBox [padRight Max $ str "Status"]

mainViewPort :: String -> Widget ()
mainViewPort url =
  border $
    topBar url
      <=> hBorder
      <=> Intro.view
      <=> hBorder
      <=> statusLine

ui :: String -> Widget ()
ui url =
  withBorderStyle unicodeRounded $
    joinBorders $
      mainViewPort url
