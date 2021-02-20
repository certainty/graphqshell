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
import Shell.Components.Shared
import Shell.Configuration
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
application :: EventChan -> Theme -> App Main.State Event ComponentName
application chan theme =
  App
    { appDraw = Main.view,
      appChooseCursor = neverShowCursor,
      appHandleEvent = Main.update chan,
      appAttrMap = const (themeToAttrMap theme),
      appStartEvent = pure
    }

applicationVTY :: IO V.Vty
applicationVTY = V.mkVty V.defaultConfig

-- Background thread to run tick events fed into the system
-- This is required to have the UI updated even though no user interaction occurred
-- for example to update progress indicators.
startTickThread ::
  -- | The tick rate in microseconds
  Int ->
  -- | 'ThreadId' of the tick thread and the channel that is used to write to
  IO (ThreadId, EventChan)
startTickThread tickRate = do
  chan <- BCh.newBChan 5
  thread <- forkIO $
    forever $ do
      BCh.writeBChan chan Tick
      threadDelay tickRate
  pure (thread, chan)
