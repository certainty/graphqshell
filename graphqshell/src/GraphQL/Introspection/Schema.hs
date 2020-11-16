{-| Main interface to interact with the schema as exposed via introspection

The introspection schema provides a set of functions that allow
us to answer questions about remote types. This includes
things like searching for types or fields, introspecting types of fields,
arguments, descriptions etc.

Examples:

```
```     

-}

module GraphQL.Introspection.Schema
  (
    -- re-exports
    GraphQL.Introspection.Schema.Types.Schema

    -- exports
    
  , searchType
  , queryType
  , derefType
  )
where
import Relude
import qualified Data.FuzzySet as FS
import qualified Data.HashMap.Strict as Dict
import GraphQL.Introspection.Schema.Types

--- Get information about the schema

derefType :: TypeReference -> Schema -> Maybe GraphQLType
derefType (NamedType ref) schema = Dict.lookup ref (universe schema)
derefType (ListOf ref) schema    = derefType ref schema
derefType (NonNullOf ref) schema = derefType ref schema
derefType _ _ = Nothing

queryType :: Schema -> Maybe GraphQLType
queryType schema = derefType (query schema) schema

-- | fuzzy search for types
searchType :: Text -> Schema -> [(Double, TypeReference)]
searchType needle schema = map (\(score, tpeName) ->  (score, NamedType tpeName)) matches
  where
    matches = FS.get (fuzzTypes schema) needle
