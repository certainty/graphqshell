module Infrastructure.TuiEngine
  ( Configuration (..),
    Continuation (..),
    Event (..),
    Component (..),
    IOHandler,
    Engine (..),
    run,
  )
where

import Brick
import qualified Brick.BChan as BCh
import Brick.Themes
import Control.Concurrent
  ( ThreadId,
    forkIO,
    threadDelay,
  )
import Data.Default
import qualified Graphics.Vty as V
import Infrastructure.TuiEngine.Keys
import Relude
  ( Applicative (pure),
    Eq,
    IO,
    Int,
    Maybe (..),
    Num ((*)),
    Ord,
    Show,
    const,
    forever,
    liftIO,
    ($),
    (>>),
    (>>=),
  )

data Configuration = Configuration
  { _tickRateMicroseconds :: Int,
    _theme :: Theme
  }

instance Default Configuration where
  def =
    Configuration
      { _tickRateMicroseconds = 200 * 1000,
        _theme = newTheme V.defAttr []
      }

data Event event
  = EventTick
  | EventApp event
  | EventInputKey Key
  deriving (Eq, Show)

type EventChan evt = (BCh.BChan (Event evt))

type ActionChan act = (BCh.BChan act)

data Continuation state action event
  = Notify state event
  | Perform state action
  | PerformAndNotify state action event
  | Continue state
  | Quit state

data Component state action event name = Component
  { attributes :: state -> AttrMap,
    initial :: Continuation state action event,
    update :: state -> Event event -> Continuation state action event,
    -- | components might be renderable
    view :: Maybe (state -> [Widget name])
  }

type IOHandler action event = action -> IO (Maybe event)

data Engine state action event name = Engine
  { ioHandler :: IOHandler action event,
    rootComponent :: Component state action event name
  }

run :: (Ord name) => Engine state action event name -> Configuration -> IO state
run engine config = do
  -- TODO: make sure the threads are killed when this function exits
  (_eventThread, eventChan) <- startTickThread (_tickRateMicroseconds config)
  (_ioThread, ioChan) <- startIOThread (ioHandler engine) eventChan
  vty <- engineVTY
  initialState <- applyContinuation ioChan eventChan (initial root)
  customMain vty engineVTY (Just eventChan) (engineApplication root ioChan eventChan (_theme config)) initialState
  where
    root = rootComponent engine
    engineVTY = V.mkVty V.defaultConfig

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
    Just translatedKey -> continuationToEventM actionChan eventChan (update rootComponent state (EventInputKey translatedKey))
    Nothing -> continue state
engineUpdate rootComponent actionChan eventChan state (AppEvent evt) = continuationToEventM actionChan eventChan (update rootComponent state evt)
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
engineView rootComponent state = case view rootComponent of
  Just component -> component state
  Nothing -> []

-- Background thread to run tick events fed into the system
-- This is required to have the UI updated even though no user interaction occurred
-- for example to update progress indicators.
startTickThread ::
  -- | The tick rate in microseconds
  Int ->
  -- | 'ThreadId' of the tick thread and the channel that is used to write to
  IO (ThreadId, EventChan event)
startTickThread tickRate = do
  chan <- BCh.newBChan 5
  thread <- forkIO $
    forever $ do
      BCh.writeBChan chan EventTick
      threadDelay tickRate
  pure (thread, chan)

-- Action handling

-- The IO thread takes care of executing io actions in a separate thread
startIOThread :: IOHandler action event -> EventChan event -> IO (ThreadId, ActionChan action)
startIOThread ioHandler eventChan = do
  chan <- BCh.newBChan 5
  thread <- forkIO $
    forever $ do
      action <- BCh.readBChan chan
      event <- ioHandler action
      case event of
        Nothing -> pure ()
        Just ioEvent -> BCh.writeBChan eventChan (EventApp ioEvent)
  pure (thread, chan)
