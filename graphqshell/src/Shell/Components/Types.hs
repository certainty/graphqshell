-- | Shared types for all components
module Shell.Components.Types where

import qualified Brick.BChan as BCh
import Relude

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
  deriving (Eq, Show)

type EventChan = (BCh.BChan Event)
