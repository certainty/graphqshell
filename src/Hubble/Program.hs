{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Hubble.Program
  ( startProgram,
    ProgramOptions (..),
    TickRate (..),
    Message (..),
    Model,
    mkModel,
    mState,
    update,
    view,
    init,
    exit,
    abort,
    cont,
    perform,
    performBatch,
    UpdateM,
    LogMessage (..),
    LogSeverity (..),
    logInfo,
    logWarning,
    logError,
    logMessages,
  )
where

import Brick (App (..), BrickEvent (AppEvent), EventM, Next, Widget, continue, customMain, halt)
import Brick.AttrMap (AttrMap, attrMap)
import qualified Brick.BChan as BCh
import Brick.Main (neverShowCursor)
import Brick.Types (BrickEvent (VtyEvent))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (waitBoth, withAsync)
import Control.Monad.Except (throwError)
import Data.Default
import Data.Maybe
import Data.Vector (cons)
import qualified Data.Vector as Vector
import Graphics.Vty (defAttr)
import qualified Graphics.Vty as V
import Hubble.CommandHandler hiding (logError, logInfo, logWarning)
import Hubble.Internal.Types (Command (Cmd, Flush), CommandChannel, LogMessage (LogMessage), LogSeverity (..), Message (AppMsg, CommandFailed, KeyMsg, LogMsg, ResizeMsg, TickMsg), MessageChannel, Model, ProgramError (ProgramError), TickRate (Micros, Millis), UpdateM, UpdateState (UpdateState, _usExit, _usLogMessages), mInit, mState, mUpdate, mView, mkModel, runUpdate, usLogMessages)
import Hubble.KeyMap (Key (Key))
import Lens.Micro.Platform ((%~), (.~), (^.))
import Relude hiding (init, state)

update :: Model s cmd msg -> Message msg -> UpdateM (Model s cmd msg, [cmd])
update m msg = do
  (updatedModel, commands) <- updateImpl (m ^. mState) msg
  pure (m & mState .~ updatedModel, commands)
  where
    updateImpl = m ^. mUpdate

view :: Model s cmd msg -> Widget ()
view m = m ^. mView $ (m ^. mState)

init :: Model s cmd msg -> [cmd]
init m = m ^. mInit

-- | Return the updated state without emitting any commands
cont :: s -> UpdateM (s, [cmd])
cont m = pure (m, [])

-- | Return updated state and perform the specified `cmd`
perform :: s -> cmd -> UpdateM (s, [cmd])
perform m cmd = pure (m, [cmd])

-- | Return updated state and perform the specified batch of `cmd`
performBatch :: s -> [cmd] -> UpdateM (s, [cmd])
performBatch s c = pure (s, c)

exit :: s -> UpdateM (s, [cmd])
exit state =
  modify (\s -> s {_usExit = True}) >> pure (state, [])

abort :: Text -> UpdateM ()
abort message = throwError (ProgramError message)

logInfo :: Text -> UpdateM ()
logInfo = log' Info

logWarning :: Text -> UpdateM ()
logWarning = log' Warning

logError :: Text -> UpdateM ()
logError = log' Error

log' :: LogSeverity -> Text -> UpdateM ()
log' severity msg = modify (\s -> s {_usLogMessages = cons (LogMessage severity msg) (_usLogMessages s)})

logMessages :: UpdateM [LogMessage]
logMessages = Vector.toList <$> gets _usLogMessages

type ProgramState m msg cmd = Either Text (Model m msg cmd, UpdateState)

data ProgramOptions = ProgramOptions
  { poTickRate :: TickRate,
    poAttrMap :: AttrMap
  }
  deriving (Show)

instance Default ProgramOptions where
  def =
    ProgramOptions
      { poTickRate = Millis 100,
        poAttrMap = attrMap defAttr []
      }

-- | Start your TUI program using the specified `Model` and IO handler
startProgram ::
  -- | The initial model
  Model m cmd msg ->
  -- | The handler of the programs commands. Use that for your IO actions. All of those actions are executed in a separate IO thread
  CommandHandler cmd msg ->
  ProgramOptions ->
  IO (Either Text (Model m cmd msg))
startProgram model commandHandler options = do
  vty <- programVTY
  messageChan <- BCh.newBChan 10
  commandChan <- BCh.newBChan 10
  stopMessages <- newTVarIO False

  withAsync (programGenerateTicks tickRate messageChan stopMessages) $ \tickThread -> do
    withAsync (programHandleCommands commandHandler commandChan messageChan stopMessages) $ \commandHandlerThread -> do
      finalState <- customMain vty programVTY (Just messageChan) (brickApplication commandChan programAttrMap) initialState
      -- Write to command handler channel to make sure the read inside the handler returns and the thread can read the tvar update.
      -- This is required because BCh.BChan doesn't support nonblocking reads
      cmdFlush commandChan
      atomically $ writeTVar stopMessages True
      waitBoth commandHandlerThread tickThread
      pure $ fst <$> finalState
  where
    programVTY = V.mkVty V.defaultConfig
    initialState = Right (model, UpdateState Nothing mempty False)
    tickRate = poTickRate options
    programAttrMap = poAttrMap options

brickApplication :: CommandChannel cmd -> AttrMap -> App (ProgramState m cmd msg) (Message msg) ()
brickApplication commandChan programAttrMap =
  App
    { appDraw = programView,
      appChooseCursor = neverShowCursor,
      appHandleEvent = programUpdate commandChan,
      appAttrMap = const programAttrMap,
      appStartEvent = pure
    }

programUpdate :: CommandChannel cmd -> ProgramState m cmd msg -> BrickEvent name (Message msg) -> EventM a (Next (ProgramState m cmd msg))
-- handle key events
programUpdate commandChan state (VtyEvent (V.EvKey key modifiers)) = programUpdate commandChan state (AppEvent (KeyMsg (Key (key, modifiers))))
-- handle resize event
programUpdate commandChan state (VtyEvent (V.EvResize h w)) = programUpdate commandChan state (AppEvent (ResizeMsg h w))
-- record log messages from the command handler
programUpdate _ (Right (model, internalState)) (AppEvent (LogMsg msg)) = do
  let updatedInternalState = internalState & usLogMessages %~ cons msg
  continue $ Right (model, updatedInternalState)
-- handle app events
programUpdate commandChan (Right (model, internalState)) (AppEvent msg) = do
  case runUpdate internalState $ update model msg of
    (Left programError, _) -> halt (Left (show programError))
    (Right (updatedModel, commands), updatedInternalState) -> do
      _ <- liftIO $ traverse (cmdSend commandChan) commands
      if _usExit updatedInternalState
        then halt $ Right (updatedModel, updatedInternalState)
        else continue $ Right (updatedModel, updatedInternalState)
programUpdate _commandChan programState _event = continue programState

programView :: ProgramState m msg cmd -> [Widget ()]
programView (Right (model, _)) = [view model]
programView _ = error "BUG in programView"

programHandleCommands :: CommandHandler cmd msg -> CommandChannel cmd -> MessageChannel msg -> TVar Bool -> IO ()
programHandleCommands cmdHandler commandChan messageChan stopMessages = do
  ifM
    (readTVarIO stopMessages)
    (pure ())
    ( liftIO $
        BCh.readBChan commandChan
          >>= dispatchCommand . unwrapCmd
          >> programHandleCommands cmdHandler commandChan messageChan stopMessages
    )
  where
    dispatchCommand Nothing = pure ()
    dispatchCommand (Just cmd) = do
      commandResult <- runCommandHandler handlerEnv (cmdHandler cmd)
      case commandResult of
        Left err -> dispatchMessage (CommandFailed (show err))
        Right _ -> pure ()
    dispatchMessage = BCh.writeBChan messageChan
    handlerEnv = CommandHandlerEnv commandChan messageChan

programGenerateTicks :: TickRate -> MessageChannel msg -> TVar Bool -> IO ()
programGenerateTicks tickRate messageChan stopMessages = do
  ifM
    (readTVarIO stopMessages)
    (pure ())
    ( liftIO $
        BCh.writeBChan messageChan TickMsg
          >> threadDelay tickRateMS
          >> programGenerateTicks tickRate messageChan stopMessages
    )
  where
    tickRateMS = case tickRate of
      Millis n -> n * 1000
      Micros n -> n

cmdFlush :: CommandChannel cmd -> IO ()
cmdFlush chan = BCh.writeBChan chan Flush

cmdSend :: CommandChannel cmd -> cmd -> IO ()
cmdSend chan cmd = BCh.writeBChan chan (Cmd cmd)

unwrapCmd :: Command cmd -> Maybe cmd
unwrapCmd (Cmd cmd) = Just cmd
unwrapCmd _ = Nothing
