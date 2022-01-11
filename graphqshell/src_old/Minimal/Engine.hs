module Minimal.Engine where

import Control.Monad.Logger (MonadLogger, WriterLoggingT)
import Relude

data Action appState event = Action
  { actionDescription :: Text,
    actionHandler :: appState -> IO (Maybe event)
  }

data EventHandlerState appState event = EventHandlerState
  { ehsAppState :: appState,
    ehsActions :: [Action appState event]
  }

newtype EventHandler s a = EventHandler {runEventHandler :: StateT s (WriterLoggingT Identity) a}
  deriving (Functor, Applicative, Monad, MonadState s, MonadLogger)
