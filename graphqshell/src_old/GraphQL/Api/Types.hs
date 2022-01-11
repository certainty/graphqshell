module GraphQL.Api.Types
  ( GraphQLApi (..),
  )
where

import GraphQL.Introspection.Schema
import Relude

class (Monad m) => GraphQLApi m where
  introspect :: m Schema
