module Infrastructure.TuiEngine.IO (noopIOHandler, processIOActions, ActionChan, IOHandler) where

import qualified Brick.BChan as BCh
import Control.Monad.Trans.Loop (exit, repeatLoopT)
import Infrastructure.TuiEngine.Events
import Relude

type ActionChan act = (BCh.BChan act)

type IOHandler action event = action -> IO (Maybe event)

noopIOHandler :: IOHandler action event
noopIOHandler _ = pure Nothing

processIOActions :: ActionChan action -> EventChan event -> TVar Bool -> IOHandler action event -> IO ()
processIOActions actionChan eventChan stopEvents ioHandler =
  repeatLoopT $ do
    stop <- readTVarIO stopEvents
    if stop
      then exit
      else liftIO $ do
        action <- BCh.readBChan actionChan
        event <- ioHandler action
        case event of
          Nothing -> pure ()
          Just ioEvent -> BCh.writeBChan eventChan (EventApp ioEvent)
