module Shell.Application
  ( run
  )
where

import           Brick
import qualified Brick.BChan                   as BCh
import           Control.Concurrent             ( ThreadId
                                                , forkIO
                                                , threadDelay
                                                )
import qualified GraphQL.API                   as API
import qualified Graphics.Vty                  as V
import           Relude                  hiding ( state )
import qualified Shell.Components.Introspection
                                               as Intro
import qualified Shell.Components.Main         as Main
import           Shell.Components.Types
import           Shell.Configuration
import           Lens.Micro.Platform            ( (^.) )

-- Main entry point to run the application
run :: ApplicationConfig -> Int -> IO Main.State
run config tickRate = do
  appState <- initialState (API.mkApiSettings (config ^. defaultEndpointConfig))
  (_, chan) <- startTickThread tickRate
  vty <- applicationVTY

  customMain vty applicationVTY (Just chan) application appState
 where
  initialState settings = Main.initialState settings <$> initialSchema settings
  initialSchema settings = API.runApiIO settings API.introspect

application :: App Main.State Main.Event ComponentName
application = App { appDraw         = Main.view
                  , appChooseCursor = neverShowCursor
                  , appHandleEvent  = Main.update
                  , appAttrMap      = const applicationAttrMap
                  , appStartEvent   = pure
                  }

applicationAttrMap :: AttrMap
applicationAttrMap = attrMap V.defAttr Intro.attributes

applicationVTY :: IO V.Vty
applicationVTY = V.mkVty V.defaultConfig

-- Background thread to run tick events fed into the system
-- This is required to have the UI updated even though no user interaction occurred
-- for example to update progress indicators.
startTickThread
  ::
  -- | The tick rate in microseconds
     Int
  ->
  -- | 'ThreadId' of the tick thread and the channel that is used to write to
     IO (ThreadId, BCh.BChan Main.Event)
startTickThread tickRate = do
  chan   <- BCh.newBChan 5
  thread <- forkIO $ forever $ do
    BCh.writeBChan chan Main.Tick
    threadDelay tickRate
  pure (thread, chan)
