{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hubble.Internal.Types where

import Brick (Widget)
import qualified Brick.BChan as BCh
import Control.Monad.Except (MonadError)
import Data.Maybe
import Data.Vector
import Hubble.KeyMap (Key)
import Lens.Micro.Platform
import Relude

data TickRate = Millis Int | Micros Int deriving (Eq, Show)

data Message appMsg
  = -- | application specifc messages
    AppMsg appMsg
  | -- | tick message is emitted regularly in the interval passt to startProgram
    TickMsg
  | -- | a key has been pressed
    KeyMsg Key
  | -- | a resize event
    ResizeMsg Int Int
  | -- | Emit a message to be logged
    LogMsg LogMessage
  | -- | when a command handler failed this message is emitted
    CommandFailed Text
  deriving (Eq, Show)

data Command cmd = Cmd cmd | Flush

data LogSeverity = Info | Warning | Error deriving (Eq, Show, Ord)

data LogMessage = LogMessage LogSeverity Text deriving (Eq, Show)

data UpdateState = UpdateState
  { _usRrrorMessage :: Maybe Text,
    _usLogMessages :: Vector LogMessage,
    _usExit :: Bool
  }

makeLenses ''UpdateState

newtype ProgramError = ProgramError Text deriving (Eq, Show)

instance Exception ProgramError

-- We make sure that we're introducing the UpdateMonad already which allows us to add monadic effects later on
newtype UpdateM a = UpdateM {unUpdate :: ExceptT ProgramError (StateT UpdateState Identity) a}
  deriving (Functor, Applicative, Monad, MonadState UpdateState, MonadError ProgramError)

runUpdate :: UpdateState -> UpdateM a -> (Either ProgramError a, UpdateState)
runUpdate s v = runIdentity . runStateT (runExceptT (unUpdate v)) $ s

type UpdateHandler s cmd msg = s -> Message msg -> UpdateM (s, [cmd])

data Model s cmd msg = Model
  { _mState :: s,
    _mInit :: [cmd],
    _mUpdate :: UpdateHandler s cmd msg,
    _mView :: s -> Widget ()
  }

makeLenses ''Model

mkModel ::
  s ->
  [cmd] ->
  UpdateHandler s cmd msg ->
  (s -> Widget ()) ->
  Model s cmd msg
mkModel = Model

type CommandChannel cmd = BCh.BChan (Command cmd)

type MessageChannel msg = BCh.BChan (Message msg)
