module Hubble.CommandHandler where

import qualified Brick.BChan as BCh
import Control.Exception.Safe (MonadThrow)
import Hubble.Internal.Types (Command (Cmd), CommandChannel, LogMessage (LogMessage), LogSeverity (..), Message (AppMsg, LogMsg), MessageChannel)
import Relude

newtype CommandHandlerError = CommandHandlerError Text deriving (Eq, Show)

instance Exception CommandHandlerError

data CommandHandlerEnv cmd msg = CommandHandlerEnv
  { _chCommandChannel :: CommandChannel cmd,
    _chMessageChannel :: MessageChannel msg
  }

newtype CommandHandlerM c m a = ComandHandlerM
  { runCommandHandlerM :: ReaderT (CommandHandlerEnv c m) (ExceptT CommandHandlerError IO) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader (CommandHandlerEnv c m))

sendCommand :: cmd -> CommandHandlerM cmd msg ()
sendCommand cmd = do
  chan <- asks _chCommandChannel
  liftIO $ BCh.writeBChan chan $ Cmd cmd

sendMessage :: msg -> CommandHandlerM cmd msg ()
sendMessage msg = do
  chan <- asks _chMessageChannel
  liftIO $ BCh.writeBChan chan $ AppMsg msg

runCommandHandler :: CommandHandlerEnv cmd msg -> CommandHandlerM cmd msg a -> IO (Either CommandHandlerError a)
runCommandHandler env action = runExceptT $ runReaderT (runCommandHandlerM action) env

type CommandHandler cmd msg = cmd -> CommandHandlerM cmd msg ()

logInfo :: Text -> CommandHandlerM cmd msg ()
logInfo = sendLogMessage Info

logWarning :: Text -> CommandHandlerM cmd msg ()
logWarning = sendLogMessage Warning

logError :: Text -> CommandHandlerM cmd msg ()
logError = sendLogMessage Error

sendLogMessage :: LogSeverity -> Text -> CommandHandlerM cmd msg ()
sendLogMessage severity msg = do
  chan <- asks _chMessageChannel
  liftIO $ BCh.writeBChan chan $ LogMsg (LogMessage severity msg)
