module GQShell.Core.GraphQL.API
  ( introspect,
    GraphQLApi,
  )
where

import GQShell.Core.GraphQL.Introspection.Schema (Schema)
import Relude

class (Monad m) => GraphQLApi m where
  introspect :: m Schema
