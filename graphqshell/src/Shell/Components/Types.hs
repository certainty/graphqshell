-- | Shared types for all components
module Shell.Components.Types where

import Relude

data ComponentName = MainComponent | IntrospectorComponent | CommandBarComponent deriving (Eq, Ord, Show)

-- Commands that can be invoked via the command bar
data CommandBarCommand
  = CmdQuit
  | CmdNoop
  deriving (Eq, Show)
