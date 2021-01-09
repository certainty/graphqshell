-- | Share types for all components
module Shell.Components.Types where

import Relude

-- | A resource name for each component that we use
-- Currently just () but will be refactored later
data ComponentName = MainComponent | IntrospectorComponent | CommandBarComponent deriving (Eq, Ord, Show)

data Command = KeyCmdQuit
