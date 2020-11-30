{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Shell.Application (run) where

import Brick
import qualified Brick.BChan as BCh
import qualified Brick.Focus as Focus
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import qualified GraphQL.API as API
import GraphQL.Introspection.Schema (Schema, query)
import qualified Graphics.Vty as V
import Lens.Micro.Platform (Lens', makeLenses, (.~), (^.))
import Relude hiding (state)
import qualified Shell.Components.Introspection as Intro
import Shell.Components.Types
import Text.URI (renderStr)

-- | The application state holds global data and component specific data.
-- The application will delegate updates to the currently active component automatically.
data ApplicationState = ApplicationState
  { -- | The 'Schema' of the currently connected GraphQL API
    _stSchema :: Schema,
    -- | settings for the 'API' client
    _stApiSettings :: API.ApiSettings,
    -- | Manage which component has the focus
    _stFocus :: !(Focus.FocusRing ComponentName),
    -- | State for the introspector component
    _stIntrospectorState :: Intro.State
  }

makeLenses ''ApplicationState

mkInitialState :: API.ApiSettings -> Schema -> ApplicationState
mkInitialState settings schema = ApplicationState schema settings (Focus.focusRing components) (Intro.mkState schema (query schema))
  where
    components = [()]

-- Application Events
data ApplicationEvent
  = IntrospectorEvent Intro.Event
  | Tick
  deriving (Eq, Ord, Show)

-- Main entry point to run the application
run :: Text -> Int -> IO ApplicationState
run url tickRate = do
  (_, chan) <- startTickThread tickRate
  apiSettings' <- API.mkApiSettings url
  schema' <- API.runApiIO apiSettings' API.introspect
  initialVty <- buildVty
  customMain initialVty buildVty (Just chan) (makeApplication attrs) (mkInitialState apiSettings' schema')
  where
    buildVty = V.mkVty V.defaultConfig
    attrs = attrMap V.defAttr Intro.attributes

-- Background thread to run tick events fed into the system
-- This is required to have the UI updated even though no user interaction occurred
-- for example to update progress indicators.
startTickThread ::
  -- | The tick rate in microseconds
  Int ->
  -- | 'ThreadId' of the tick thread and the channel that is used to write to
  IO (ThreadId, BCh.BChan ApplicationEvent)
startTickThread tickRate = do
  chan <- BCh.newBChan 5
  thread <- forkIO $
    forever $ do
      BCh.writeBChan chan Tick
      threadDelay tickRate
  pure (thread, chan)

makeApplication :: AttrMap -> App ApplicationState ApplicationEvent ComponentName
makeApplication attrs =
  App
    { appDraw = draw,
      appChooseCursor = neverShowCursor,
      appHandleEvent = update,
      appAttrMap = const attrs,
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
topBar state = hBox [padRight Max $ padLeft (Pad 1) $ txt (toText url)]
  where
    url = renderStr . API.apiURI $ state ^. stApiSettings

mainViewPort :: ApplicationState -> Widget ComponentName
mainViewPort state =
  border $
    topBar state
      <=> hBorder
      <=> tabLine
      <=> hBorder
      <=> Intro.view (state ^. stIntrospectorState)
      <=> hBorder
      <=> statusLine

tabLine :: Widget ComponentName
tabLine = hBox [padRight Max $ padLeft (Pad 1) $ txt "[ Introspector ] | [ Query ]"]

statusLine :: Widget ComponentName
statusLine = hBox [padRight Max $ padLeft (Pad 1) $ str "C-c: Exit"]
