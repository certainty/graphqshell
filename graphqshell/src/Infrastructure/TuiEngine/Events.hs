module Infrastructure.TuiEngine.Events (Event (..), EventChan, TickRate (..), generateTickEvents) where

import qualified Brick.BChan as BCh
import Control.Concurrent
import Control.Monad.Trans.Loop (exit, repeatLoopT)
import Data.Default
import Infrastructure.TuiEngine.Keys
import Relude

data TickRate = Millis Int | Seconds Int deriving (Eq, Ord, Show)

instance Default TickRate where
  def = Millis 100

data Event event
  = EventTick
  | EventApp event
  | EventInputKey Key
  deriving (Eq, Show)

type EventChan evt = (BCh.BChan (Event evt))

generateTickEvents :: TickRate -> EventChan event -> TVar Bool -> IO ()
generateTickEvents tickRate eventChan stopEvents = repeatLoopT $ do
  stop <- readTVarIO stopEvents
  if stop
    then exit
    else liftIO $ do
      BCh.writeBChan eventChan EventTick
      threadDelay (tickRateToMicroseconds tickRate)

tickRateToMicroseconds :: TickRate -> Int
tickRateToMicroseconds (Millis ms) = ms * 1000
tickRateToMicroseconds (Seconds s) = s * 1000000
