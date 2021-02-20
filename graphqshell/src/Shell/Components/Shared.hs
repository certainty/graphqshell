{-# LANGUAGE RankNTypes #-}

module Shell.Components.Shared where

import Brick
import qualified Brick.BChan as BCh
import Control.Applicative
import GraphQL.Introspection.Schema (GraphQLType)
import Lens.Micro.Platform
  ( Lens',
    set,
    view,
  )
import Relude hiding
  ( state,
  )

data ComponentName = MainComponent | IntrospectorComponent | CommandBarComponent deriving (Eq, Ord, Show)

-- Commands that can be invoked via the command bar
data CommandBarCommand
  = CmdQuit
  | CmdNoop
  deriving (Eq, Show)

-- Application Events for all components
data Event
  = KeyCommand CommandBarCommand
  | Tick
  | SelectedTypeChanged GraphQLType
  deriving (Eq, Show)

type EventChan = (BCh.BChan Event)

-- Utilities used accross components
--
-- Relay the update to a sub component
relayUpdate ::
  a -> -- the state
  Lens' a b -> -- the lens to focus the target state
  (e -> b -> EventM n (Next b)) -> -- the event handler
  e -> -- the event
  EventM n (Next a) -- the result
relayUpdate st componentStateLens componentUpdate evt = do
  nextComponentState <- componentUpdate evt (view componentStateLens st)
  pure $ (\newState -> set componentStateLens newState st) <$> nextComponentState

emitEvent :: BCh.BChan e -> s -> e -> EventM n (Next s)
emitEvent chan state event =
  suspendAndResume (liftIO $ BCh.writeBChanNonBlocking chan event >> pure state)
