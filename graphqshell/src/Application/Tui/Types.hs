{-# LANGUAGE TemplateHaskell #-}

module Application.Tui.Types where

import Application.Tui.Keymap
import Application.Tui.Screen
import Control.Monad.Logger (LogLine, MonadLogger, WriterLoggingT, runWriterLoggingT)
import qualified Data.Vector as V
import Optics
import Relude

-- Actions
newtype TuiAction a = Action {runAction' :: StateT TuiState (WriterLoggingT Identity) a}
  deriving (Functor, Applicative, Monad, MonadState TuiState, MonadLogger)

-- Commands
data TuiCommand = Command deriving (Eq, Show)

-- Events
data TuiEvent = TuiEvent

-- Config

data TuiConfig = TuiConfig
  { _tcDefaultView :: ViewName,
    _tcKeyMaps :: Map ViewName (KeyMap (TuiAction ()))
  }

makeLenses ''TuiConfig

-- State

data TuiState = TuiState
  { _tsConfig :: TuiConfig,
    _tsViews :: ViewState,
    _tsFocusedViewName :: ViewName,
    _tsCommands :: V.Vector TuiCommand,
    _tsEvents :: V.Vector TuiEvent
  }

makeLenses ''TuiState

-- Channels
type CommandChannel = Bch.BChan TuiCommand

type EventChannel = Bch.BChan TuiEvent
