-- | Share types for all components
module Shell.Components.Types where

-- | A resource name for each component that we use
-- Currently just () but will be refactored later
type ComponentName = () -- Main | Introspector deriving (Eq, Ord, Show)
