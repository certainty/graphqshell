{-# LANGUAGE RankNTypes #-}

-- | This is our own representation on how the program may resume
--
-- Brick provides something similar and in fact we translate our continuation
-- back to brick's continuation but contrary to brick we usually don't want to run
-- arbitrary IO in the update functions. Instead we have them return IO actions
-- which might be executed in parallel or concurrently. This is really just
-- a thin translation layer though.

module Shell.Continuation
  ( keepGoing
  , stopIt
  , concurrently
  , adaptToBrick
  , Action(..)
  , Continuation
  , updateComponent
  )
where
import qualified Brick.BChan                   as BCh
import           Relude                                                          hiding ( state
                                                                                        )
import           Brick
import           Lens.Micro.Platform                                                    ( Lens'
                                                                                        , set
                                                                                        , view
                                                                                        )
import           Shell.Components.Types                                                 ( )


-- An action is parameterized over the event that it might emit
newtype Action e = Action { runAction :: IO e } deriving (Functor)


data Continuation e s =
  -- | Continue with this state and don't perform any IO
 Continue s
 -- | Stop execution again without performing any IO
 | Stop s
  -- | Run the action concurrently
 | Concurrently s (Action e)

instance Bifunctor Continuation where
  bimap _  fs (Continue s      ) = Continue (fs s)
  bimap _  fs (Stop     s      ) = Stop (fs s)
  bimap fa fs (Concurrently s a) = (Concurrently (fs s) (fa <$> a))

instance Functor (Continuation e) where
  fmap f (Continue s      ) = Continue (f s)
  fmap f (Stop     s      ) = Stop (f s)
  fmap f (Concurrently s a) = (Concurrently (f s) a)

keepGoing :: s -> EventM n (Continuation e s)
keepGoing = pure . Continue

stopIt :: s -> EventM n (Continuation e s)
stopIt = pure . Stop

concurrently :: s -> IO e -> EventM n (Continuation e s)
concurrently s a = pure $ (Concurrently s (Action a))

-- | Adapt our own continuation to the brick continuation
-- |
-- | All functions beneath that should us a the following signature for update
-- |
-- | @
-- | update :: State -> BrickEvent n Event -> EventM n (Continuation State Event)
-- | @
adaptToBrick :: BCh.BChan e -> Continuation e s -> EventM n (Next s)
adaptToBrick _ (Continue s) = continue s
adaptToBrick _ (Stop     s) = halt s
adaptToBrick chan (Concurrently s action) =
  (liftIO $ (runAction action) >>= BCh.writeBChanNonBlocking chan) >> continue s


-- | Helper to update components more conveniently
-- updateComponent
--   :: a
--   -> Lens' a b
--   -> (e -> e2)
--   -> (b -> e -> EventM n (Continuation e b))
--   -> e
--   -> EventM n (Continuation e2 a)
-- updateComponent state target eventUpdate componentUpdate event = do
--   cont <- componentUpdate (view target state) event
--   let newCont = bimap eventUpdate stateUpdate cont
--   pure newCont
--   where stateUpdate s = set target s state


updateComponent
  ::
  -- | The main state of this component
     state
  -- | The lens to focus the state of the subcomponent
  -> Lens' state componentState
  -- | A function mapping the events of the underlying component
  -> (componentEvent -> event)
  -- | The update function of the subcomponent
  -> (  componentState
     -> (BrickEvent resource componentEvent)
     -> EventM resource (Continuation componentEvent componentState)
     )
  -- | The event to feed into the update function of the subcomponent
  -> BrickEvent resource componentEvent
  -> EventM resource (Continuation event state)
updateComponent state componentStateLens componentEventMap componentUpdate componentEvent
  = do
    cont <- componentUpdate (view componentStateLens state) componentEvent
    let newCont = bimap
          componentEventMap
          (\newComponentState -> set componentStateLens newComponentState state)
          cont
    pure newCont
