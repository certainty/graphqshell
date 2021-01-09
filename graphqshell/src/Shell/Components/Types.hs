-- | Share types for all components
module Shell.Components.Types where

import Relude

-- | A resource name for each component that we use
-- Currently just () but will be refactored later
data ComponentName = MainComponent | IntrospectorComponent | GlobalCommandBarComponent | ContextCommandBarComponent deriving (Eq, Ord, Show)

data Command = Global GlobalCommand | Context ContextCommand
  deriving (Eq, Show)

data GlobalCommand = CmdQuit deriving (Eq, Show)

data ContextCommand = Noop deriving (Eq, Show)
