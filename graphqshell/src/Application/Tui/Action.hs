module Application.Tui.Action where

import Application.Tui.Types
import Control.Monad.Logger (LogLine, MonadLogger, WriterLoggingT, runWriterLoggingT)
import qualified Data.Vector as V
import Relude

runAction :: TuiAction a -> TuiState -> ((a, TuiState), [LogLine])
runAction (Action action) = runIdentity . runWriterLoggingT . runStateT action

-- Add an IO command to be executed
scheduleCommand :: TuiCommand -> TuiAction ()
scheduleCommand cmd = do
  modify $ \s -> s {_tsCommands = V.cons cmd (_tsCommands s)}
