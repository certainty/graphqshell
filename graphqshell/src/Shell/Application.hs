{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Shell.Application (runShell) where

import Brick
import qualified Brick.BChan as BCh
import qualified Brick.Focus as Focus
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import qualified GraphQL.API as API
import GraphQL.Introspection.Schema (Schema)
import qualified Graphics.Vty as V
import Lens.Micro (Lens', (&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Relude hiding (state)
import qualified Shell.Components.Introspection as Intro
import Text.URI (renderStr)

-- | A resource name for each component that we use
-- Currently just () but will be refactored later
type ComponentName = () -- Main | Introspector deriving (Eq, Ord, Show)

-- Welcome the app-state
data ApplicationState = ApplicationState
  { _stSchema :: Schema,
    _stApiSettings :: API.ApiSettings,
    _stFocus :: !(Focus.FocusRing ComponentName),
    -- Component states
    _stIntrospectorState :: Intro.State
  }

makeLenses ''ApplicationState

mkInitialState :: API.ApiSettings -> Schema -> ApplicationState
mkInitialState settings schema = ApplicationState schema settings (Focus.focusRing components) (Intro.mkState schema)
  where
    components = [()]

-- Application Events
data ApplicationEvent
  = IntrospectorEvent Intro.Event
  | Tick
  deriving (Eq, Ord, Show)

runShell :: String -> Int -> IO ApplicationState
runShell url tickRate = do
  chan <- BCh.newBChan 5
  _ <- startTickThread chan tickRate
  apiSettings' <- API.mkApiSettings (toText url)
  schema' <- API.runApiIO apiSettings' API.introspect
  initialVty <- buildVty
  customMain initialVty buildVty (Just chan) makeApplication (mkInitialState apiSettings' schema')
  where
    buildVty = V.mkVty V.defaultConfig

-- Background thread to run tick events fed into the system
startTickThread :: BCh.BChan ApplicationEvent -> Int -> IO ThreadId
startTickThread chan tickRate = forkIO $
  forever $ do
    BCh.writeBChan chan Tick
    threadDelay tickRate

makeApplication :: App ApplicationState ApplicationEvent ComponentName
makeApplication =
  App
    { appDraw = draw,
      appChooseCursor = neverShowCursor,
      appHandleEvent = update,
      appAttrMap = const $ attrMap V.defAttr [],
      appStartEvent = pure
    }

update :: ApplicationState -> BrickEvent ComponentName ApplicationEvent -> EventM ComponentName (Next ApplicationState)
-- Key events
update s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt s
update s (VtyEvent evt) = updateComponent s stIntrospectorState Intro.update (VtyEvent evt)
-- App events
update s (AppEvent (IntrospectorEvent event)) = updateComponent s stIntrospectorState Intro.update (AppEvent event)
update s _ = continue s

-- Run update function for a component
updateComponent :: a -> Lens' a b -> (b -> e -> EventM n (Next b)) -> e -> EventM n (Next a)
updateComponent state target handler event = do
  newVal <- handler (state ^. target) event
  pure $ (\newState -> state & target .~ newState) <$> newVal

draw :: ApplicationState -> [Widget ComponentName]
draw state = [mainWidget state]

mainWidget :: ApplicationState -> Widget ComponentName
mainWidget state =
  withBorderStyle unicodeRounded $
    joinBorders $
      mainViewPort state

topBar :: ApplicationState -> Widget ComponentName
topBar state = hBox [padRight Max $ str $ "connected to " ++ url]
  where
    url = renderStr . API.apiURI $ state ^. stApiSettings

mainViewPort :: ApplicationState -> Widget ComponentName
mainViewPort state =
  border $
    topBar state
      <=> hBorder
      <=> Intro.view (state ^. stIntrospectorState)
      <=> hBorder
      <=> statusLine

statusLine :: Widget ComponentName
statusLine = hBox [padRight Max $ str "C-c: Exit"]
