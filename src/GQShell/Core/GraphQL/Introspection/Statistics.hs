{-# LANGUAGE TemplateHaskell #-}

module GQShell.Core.GraphQL.Introspection.Statistics where

import qualified Data.Map as Map
import GQShell.Core.GraphQL.Introspection.Schema (Schema, universe)
import GQShell.Core.GraphQL.Introspection.Schema.Types (GraphQLType)
import qualified GQShell.Core.GraphQL.Introspection.Schema.Types as GQL
import Lens.Micro.Platform (makeLenses)
import Relude hiding (universe)

data GraphQLKind = Scalar | Object | Interface | Union | Enum | InputObject deriving (Eq, Show, Ord)

type TypeCardinalities = Map GraphQLKind Int

data Statistics = Statistics
  { _statsTypeCardinality :: Int,
    _statsInputTypeCardinality :: Int,
    _statsOutputTypeCardinality :: Int,
    _statsOrphanedTypeCardinality :: Int
  }
  deriving (Eq, Show)

makeLenses ''Statistics

computeStatistics :: Schema -> Statistics
computeStatistics s = Statistics totalTypes inputTypes outputTypes 0
  where
    totalTypes = sum . Map.elems $ cardinalities
    inputTypes = Map.findWithDefault 0 InputObject cardinalities
    outputTypes = totalTypes - inputTypes
    cardinalities = collectCardinalities s

collectCardinalities :: Schema -> TypeCardinalities
collectCardinalities schema = foldl' collectCardinalities' emptyMap (universe schema)
  where
    emptyMap = Map.fromList [(Scalar, 0), (Object, 0), (Interface, 0), (Union, 0), (Enum, 0), (InputObject, 0)]
    collectCardinalities' :: TypeCardinalities -> GraphQLType -> TypeCardinalities
    collectCardinalities' acc t =
      case t of
        GQL.Scalar _ -> Map.adjust (+ 1) Scalar acc
        GQL.Object _ -> Map.adjust (+ 1) Object acc
        GQL.Interface _ -> Map.adjust (+ 1) Interface acc
        GQL.Union _ -> Map.adjust (+ 1) Union acc
        GQL.Input _ -> Map.adjust (+ 1) InputObject acc
        GQL.Enum _ -> Map.adjust (+ 1) Enum acc
