{-# LANGUAGE TemplateHaskell #-}

module Infrastructure.TuiEngine
  ( Continuation (..),
    Event (..),
    Component (..),
    IOHandler,
    EngineConfiguration (..),
    TickRate (..),
    run,
  )
where

import Brick
import qualified Brick.BChan as BCh
import Brick.Themes
import Control.Concurrent.Async (waitBoth, withAsync)
import qualified Graphics.Vty as V
import Infrastructure.TuiEngine.Components
import Infrastructure.TuiEngine.Events
import Infrastructure.TuiEngine.IO
import Infrastructure.TuiEngine.Keys
import Lens.Micro.Platform (makeLenses, (^.))
import Relude (Applicative (pure), Bool (False, True), IO, Maybe (..), Ord, TVar, atomically, const, fromMaybe, liftIO, writeTVar, ($), (>>), (>>=))
import Relude.Lifted (newTVarIO)

data EngineConfiguration state action event name = Configuration
  { _confTickRate :: TickRate,
    _confTheme :: Theme,
    _confIOHandler :: Maybe (IOHandler action event),
    _confMainComponent :: Component state action event name
  }

makeLenses ''EngineConfiguration

run :: (Ord name) => EngineConfiguration state action event name -> IO state
run engineConfig = do
  vty <- engineVTY
  eventChan <- BCh.newBChan 5
  actionChan <- BCh.newBChan 5
  stopEvents <- newTVarIO False
  withAsync (engineGenerateTicks engineConfig eventChan stopEvents) $ \eventThread -> do
    withAsync (engineHandleIOActions engineConfig actionChan eventChan stopEvents) $ \ioThread -> do
      -- run initial continuation
      initialState <- liftIO $ applyContinuation actionChan eventChan (_componentInitial root)
      -- run engine
      finalState <- customMain vty engineVTY (Just eventChan) (engineApplication root actionChan eventChan (engineConfig ^. confTheme)) initialState
      -- signal shutdown of event handlers
      atomically $ writeTVar stopEvents True
      waitBoth eventThread ioThread
      pure finalState
  where
    root = engineConfig ^. confMainComponent
    engineVTY = V.mkVty V.defaultConfig

engineGenerateTicks :: EngineConfiguration state action event name -> EventChan event -> TVar Bool -> IO ()
engineGenerateTicks config = generateTickEvents (config ^. confTickRate)

engineHandleIOActions :: EngineConfiguration state action event name -> ActionChan action -> EventChan event -> TVar Bool -> IO ()
engineHandleIOActions config actionChan eventChan stopEvents = processIOActions actionChan eventChan stopEvents (fromMaybe noopIOHandler (config ^. confIOHandler))

-- | Create brick application from the engine
engineApplication :: Component state action event name -> ActionChan action -> EventChan event -> Theme -> App state (Event event) name
engineApplication root actions events theme =
  Brick.App
    { appDraw = engineView root,
      appChooseCursor = neverShowCursor,
      appHandleEvent = engineUpdate root actions events,
      appAttrMap = const (themeToAttrMap theme),
      appStartEvent = pure
    }

--
-- UPDATE
--

-- | translate engine level event handling to the brick application
engineUpdate ::
  Component state action event name ->
  ActionChan action ->
  EventChan event ->
  state ->
  BrickEvent name (Event event) ->
  EventM name (Next state)
engineUpdate rootComponent actionChan eventChan state (VtyEvent (V.EvKey key mod)) = do
  case fromVTYKey key mod of
    Just translatedKey -> continuationToEventM actionChan eventChan (_componentUpdate rootComponent state (EventInputKey translatedKey))
    Nothing -> continue state
engineUpdate rootComponent actionChan eventChan state (AppEvent evt) = continuationToEventM actionChan eventChan (_componentUpdate rootComponent state evt)
engineUpdate _rootComponent _actionChan _eventChan state _ = continue state

-- | translate engine level event handling to the brick application, also taking care of dispatching actions and events
continuationToEventM :: ActionChan action -> EventChan event -> Continuation state action event -> EventM name (Next state)
continuationToEventM _actionChan _eventChan (Quit state) = halt state
continuationToEventM actionChan eventChan event = (liftIO $ applyContinuation actionChan eventChan event) >>= continue

-- | Dispatch the events / actions from the continuation and return the state
applyContinuation :: ActionChan action -> EventChan event -> Continuation state action event -> IO state
applyContinuation _actionChan eventChan (Notify state event) = BCh.writeBChan eventChan (EventApp event) >> pure state
applyContinuation actionChan _eventChan (Perform state action) = BCh.writeBChan actionChan action >> pure state
applyContinuation actionChan eventChan (PerformAndNotify state action event) =
  BCh.writeBChan actionChan action
    >> BCh.writeBChan eventChan (EventApp event)
    >> pure state
applyContinuation _actionChan _eventChan (Continue state) = pure state
applyContinuation _actionChan _eventChan (Quit state) = pure state

--
-- View
--

-- | Delegate the rendering to the root component if it has defined a view handler
engineView :: Component state action event name -> state -> [Widget name]
engineView rootComponent state = case _componentView rootComponent of
  Just component -> component state
  Nothing -> []
