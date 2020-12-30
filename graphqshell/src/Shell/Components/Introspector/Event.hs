-- | This is the event type that is shared for the whole introspector component
module Shell.Components.Introspector.Event where
import           Relude
import           GraphQL.Introspection.Schema                                           ( GraphQLType
                                                                                        )

{-
  _____                 _
 | ____|_   _____ _ __ | |_
 |  _| \ \ / / _ \ '_ \| __|
 | |___ \ V /  __/ | | | |_
 |_____| \_/ \___|_| |_|\__|

-}

data Event = SelectedTypeChanged GraphQLType
  deriving (Eq, Show)
