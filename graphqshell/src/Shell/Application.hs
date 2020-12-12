module Shell.Application
  ( run
  )
where

import           Brick
import           Brick.Themes
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
import           Lens.Micro.Platform            ( (^.) )
import           Control.Exception.Safe         ( MonadThrow
                                                , throw
                                                )
import           Shell.Configuration

data ThemeError = ThemeLoadError Text deriving (Show)
instance Exception ThemeError

-- Main entry point to run the application
run :: ApplicationConfig -> Int -> IO Main.State
run config tickRate = do
  appState <- initialState (API.mkApiSettings (config ^. defaultEndpointConfig))
  theme     <- loadTheme (config ^. defaultThemeConfig)
  _         <- print theme
  (_, chan) <- startTickThread tickRate
  vty       <- applicationVTY
  customMain vty
             applicationVTY
             (Just chan)
             (application (themeToAttrMap theme))
             appState
 where
  initialState settings = Main.initialState settings <$> initialSchema settings
  initialSchema settings = API.runApiIO settings API.introspect

application :: AttrMap -> App Main.State Main.Event ComponentName
application attrs = App { appDraw         = Main.view
                        , appChooseCursor = neverShowCursor
                        , appHandleEvent  = Main.update
                        , appAttrMap      = const attrs
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

loadTheme :: (MonadThrow m, MonadIO m) => ThemeConfig -> m Theme
loadTheme config = do
  result <- liftIO
    $ loadCustomizations (config ^. themePath) (newTheme V.defAttr [])
  case result of
    (Left  e    ) -> throw (ThemeLoadError (toText e))
    (Right theme) -> pure theme
