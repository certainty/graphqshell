{-# LANGUAGE BlockArguments #-}

module GQShell.Core.GraphQL.Introspection.Schema.Types
  ( GraphQLType (..),
    DeprecationInfo (..),
    ScalarType (..),
    ObjectType (..),
    UnionType (..),
    InterfaceType (..),
    EnumType (..),
    InputObjectType (..),
    FieldType (..),
    EnumValue (..),
    InputValue (..),
    TypeReference (..),
    HasDescription (..),
    CanBeDeprecated (..),
    HasName (..),
    NamedType (..),
  )
where

import Data.Vector (Vector)
import Relude

-- Schema

-- Type wrapper
data GraphQLType
  = Scalar ScalarType
  | Object ObjectType
  | Input InputObjectType
  | Enum EnumType
  | Interface InterfaceType
  | Union UnionType
  deriving (Eq, Show)

data DeprecationInfo
  = NotDeprecated
  | Deprecated (Maybe Text)
  deriving (Eq, Show)

-- Individual types
type Name = Text

newtype NamedType = NamedType
  { unName :: Name
  }
  deriving (Eq, Show)

type Description = Maybe Text

type Fields = Vector FieldType

type InputFields = Vector InputValue

type Interfaces = Vector NamedType

type PossibleTypes = Vector NamedType

type EnumVariants = Vector EnumValue

type Arguments = Vector InputValue

type OutputTypeReference = TypeReference

type InputTypeReference = TypeReference

type DefaultValue = Maybe Text

data ScalarType = ScalarType Name Description deriving (Eq, Show)

data ObjectType = ObjectType Name Description Fields Interfaces deriving (Eq, Show)

data UnionType = UnionType Name Description PossibleTypes deriving (Eq, Show)

data InterfaceType = InterfaceType Name Description Fields PossibleTypes deriving (Eq, Show)

data EnumType = EnumType Name Description EnumVariants deriving (Eq, Show)

data InputObjectType = InputObjectType Name Description InputFields deriving (Eq, Show)

data FieldType = FieldType Name Description DeprecationInfo Arguments OutputTypeReference deriving (Eq, Show)

data EnumValue = EnumValue Name Description DeprecationInfo deriving (Eq, Show)

data InputValue = InputValue Name Description InputTypeReference DefaultValue deriving (Eq, Show)

data TypeReference
  = ListOf TypeReference
  | NonNullOf TypeReference
  | Named NamedType
  deriving (Eq, Show)

class HasName a where
  name :: a -> Text

instance HasName ScalarType where
  name (ScalarType n _) = n

instance HasName ObjectType where
  name (ObjectType n _ _ _) = n

instance HasName UnionType where
  name (UnionType n _ _) = n

instance HasName InterfaceType where
  name (InterfaceType n _ _ _) = n

instance HasName EnumType where
  name (EnumType n _ _) = n

instance HasName InputObjectType where
  name (InputObjectType n _ _) = n

instance HasName FieldType where
  name (FieldType n _ _ _ _) = n

instance HasName GraphQLType where
  name (Scalar t) = name t
  name (Object t) = name t
  name (Enum t) = name t
  name (Interface t) = name t
  name (Union t) = name t
  name (Input t) = name t

class CanBeDeprecated a where
  isDeprecated :: a -> Bool
  deprecationReason :: a -> Maybe Text

instance CanBeDeprecated FieldType where
  isDeprecated (FieldType _ _ NotDeprecated _ _) = False
  isDeprecated _ = True
  deprecationReason (FieldType _ _ (Deprecated reason) _ _) = reason
  deprecationReason _ = Nothing

instance CanBeDeprecated EnumValue where
  isDeprecated (EnumValue _ _ NotDeprecated) = False
  isDeprecated _ = True
  deprecationReason (EnumValue _ _ (Deprecated reason)) = reason
  deprecationReason _ = Nothing

class HasDescription a where
  description :: a -> Description

instance HasDescription ScalarType where
  description (ScalarType _ d) = d

instance HasDescription ObjectType where
  description (ObjectType _ d _ _) = d

instance HasDescription UnionType where
  description (UnionType _ d _) = d

instance HasDescription InterfaceType where
  description (InterfaceType _ d _ _) = d

instance HasDescription EnumType where
  description (EnumType _ d _) = d

instance HasDescription InputObjectType where
  description (InputObjectType _ d _) = d

instance HasDescription FieldType where
  description (FieldType _ d _ _ _) = d

instance HasDescription GraphQLType where
  description (Scalar t) = description t
  description (Object t) = description t
  description (Enum t) = description t
  description (Interface t) = description t
  description (Union t) = description t
  description (Input t) = description t
