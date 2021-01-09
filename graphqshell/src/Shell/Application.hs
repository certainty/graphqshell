module Shell.Application
  ( run,
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
import qualified GraphQL.API as API
import qualified Graphics.Vty as V
import Lens.Micro.Platform
  ( (^.),
  )
import Relude hiding
  ( state,
  )
import qualified Shell.Components.Main as Main
import Shell.Components.Types
import Shell.Configuration
import Shell.Continuation
import Shell.Theme

defaultTickRate :: Int
defaultTickRate = 1 * 1000000

run :: ApplicationConfig -> IO Main.State
run config = do
  appState <- initialState apiConfig
  theme <- loadTheme configuredTheme
  (_, chan) <- startTickThread tickRate
  vty <- applicationVTY
  customMain vty applicationVTY (Just chan) (application chan theme) appState
  where
    apiConfig = API.mkApiSettings (config ^. appConfigDefaultEndpoint)
    configuredTheme = config ^. appConfigDefaultTheme
    initialState settings = initialSchema settings >>= Main.initialState settings
    initialSchema settings = API.runApiIO settings API.introspect
    tickRate = fromMaybe defaultTickRate (config ^. appConfigTickRate)

-- We delegate most of the functions to the main component,
-- which is what is shown first
application :: BCh.BChan Main.Event -> Theme -> App Main.State Main.Event ComponentName
application chan theme =
  App
    { appDraw = Main.view,
      appChooseCursor = neverShowCursor,
      appHandleEvent = applicationUpdate chan,
      appAttrMap = const (themeToAttrMap theme),
      appStartEvent = pure
    }

-- Adapt the underlying update functions to match the brick continuations
applicationUpdate ::
  BCh.BChan Main.Event ->
  Main.State ->
  BrickEvent ComponentName Main.Event ->
  EventM ComponentName (Next Main.State)
applicationUpdate chan s evt = Main.update s evt >>= adaptToBrick chan

applicationVTY :: IO V.Vty
applicationVTY = V.mkVty V.defaultConfig

-- Background thread to run tick events fed into the system
-- This is required to have the UI updated even though no user interaction occurred
-- for example to update progress indicators.
startTickThread ::
  -- | The tick rate in microseconds
  Int ->
  -- | 'ThreadId' of the tick thread and the channel that is used to write to
  IO (ThreadId, BCh.BChan Main.Event)
startTickThread tickRate = do
  chan <- BCh.newBChan 5
  thread <- forkIO $
    forever $ do
      BCh.writeBChan chan Main.Tick
      threadDelay tickRate
  pure (thread, chan)
