-- |
-- Main interface to interact with the schema as exposed via introspection
-- The introspection schema provides a set of functions that allow
-- us to answer questions about remote types. This includes
-- things like searching for types or fields, introspecting types of fields,
-- arguments, descriptions etc.
--
-- Please refer to 'GraphQL.Introspection' to learn how to introspect and API and
-- retrieve a 'GraphQL.Introspection.Schema' to work with.
module GraphQL.Introspection.Schema
  ( module GraphQL.Introspection.Schema.Types,
    SchemaBuildError (..),
    Schema,
    query,
    mutation,
    subscription,
    mkSchema,
    fromMarshalledSchema,
    lookupType,
    searchType,
  )
where

import qualified Data.Bifunctor
import qualified Data.FuzzySet as FS
import qualified Data.HashMap.Strict as Dict
import Data.Text (isPrefixOf)
import qualified Data.Vector as Vector
import GraphQL.Introspection.Marshalling.Types
import GraphQL.Introspection.Schema.Types hiding (deprecationReason, description, isDeprecated, name)
import qualified GraphQL.Introspection.Schema.Types as Types
import Relude hiding (isPrefixOf)

data SchemaBuildError
  = MissingQueryType
  | InvalidRootType Text
  | UnknownKind Text
  deriving (Eq, Show)

instance Exception SchemaBuildError

type TypeUniverse = Dict.HashMap Text GraphQLType

type QueryType = GraphQLType

type MutationType = Maybe GraphQLType

type SubscriptionType = Maybe GraphQLType

data Schema = Schema
  { query :: QueryType,
    mutation :: MutationType,
    subscription :: SubscriptionType,
    -- | The type universe of the schema
    universe :: TypeUniverse,
    -- | Fuzzy index of fields
    fuzzyTypes :: FS.FuzzySet
  }
  deriving (Eq, Show)

mkSchema :: QueryType -> MutationType -> SubscriptionType -> [GraphQLType] -> Schema
mkSchema queryType mutationType subscriptionType additionalTypes = Schema queryType mutationType subscriptionType typeUniverse typeIndex
  where
    typeIndex = FS.fromList (Dict.keys typeUniverse)
    typeUniverse = Dict.fromList (map (\tpe -> (Types.name tpe, tpe)) additionalTypes)

--- Get information about the schema
lookupType :: TypeReference -> Schema -> Maybe GraphQLType
lookupType (NamedType ref) schema = Dict.lookup ref (universe schema)
lookupType (ListOf ref) schema = lookupType ref schema
lookupType (NonNullOf ref) schema = lookupType ref schema
lookupType _ _ = Nothing

-- -- | fuzzy search for types
searchType :: Text -> Schema -> [(Double, TypeReference)]
searchType needle schema = map (Data.Bifunctor.second NamedType) matches
  where
    matches = FS.get (fuzzyTypes schema) needle

-- Build the schema from introspection data
fromMarshalledSchema :: IntrospectionSchema -> Either SchemaBuildError Schema
fromMarshalledSchema schema = mkSchema <$> queryType <*> maybeMutationType <*> maybeSubscriptionType <*> consideredTypes
  where
    queryType = fromMarshalledType (introspectionSchemaQueryType schema)
    maybeMutationType = fromMarshalledOpt $ introspectionSchemaMutationType schema
    maybeSubscriptionType = fromMarshalledOpt $ introspectionSchemaSubscriptionType schema
    consideredTypes = traverse fromMarshalledType consideredMarshalledTypes
    consideredMarshalledTypes = filter (not . isPrefixOf "__" . introspectionTypeName) (Vector.toList (introspectionSchemaTypes schema))

fromMarshalledOpt :: Maybe IntrospectionType -> Either SchemaBuildError (Maybe GraphQLType)
fromMarshalledOpt Nothing = Right Nothing
fromMarshalledOpt (Just tpe) = Just <$> fromMarshalledType tpe

fromMarshalledType :: IntrospectionType -> Either SchemaBuildError GraphQLType
fromMarshalledType tpe = fromMarshalledType' (introspectionTypeKind tpe) tpe

fromMarshalledType' :: Text -> IntrospectionType -> Either SchemaBuildError GraphQLType
fromMarshalledType' "SCALAR" tpe = Right $ Scalar (ScalarType name description)
  where
    name = introspectionTypeName tpe
    description = introspectionTypeDescription tpe
fromMarshalledType' "OBJECT" tpe = Right $ Object (ObjectType name description fields interfaces)
  where
    name = introspectionTypeName tpe
    description = introspectionTypeDescription tpe
    fields = mapOrEmpty fromMarshalledFieldType (introspectionTypeFields tpe)
    interfaces = mapOrEmpty fromMarshalledTypeRef (introspectionTypeInterfaces tpe)
fromMarshalledType' "INTERFACE" tpe = Right $ Interface (InterfaceType name description fields possibleTypes)
  where
    name = introspectionTypeName tpe
    description = introspectionTypeDescription tpe
    fields = mapOrEmpty fromMarshalledFieldType (introspectionTypeFields tpe)
    possibleTypes = mapOrEmpty fromMarshalledTypeRef (introspectionTypePossibleTypes tpe)
fromMarshalledType' "UNION" tpe = Right $ Union (UnionType name description possibleTypes)
  where
    name = introspectionTypeName tpe
    description = introspectionTypeDescription tpe
    possibleTypes = mapOrEmpty fromMarshalledTypeRef (introspectionTypePossibleTypes tpe)
fromMarshalledType' "ENUM" tpe = Right $ Enum (EnumType name description variants)
  where
    name = introspectionTypeName tpe
    description = introspectionTypeDescription tpe
    variants = mapOrEmpty fromMarshalledEnumValue (introspectionTypeEnumValues tpe)
fromMarshalledType' "INPUT_OBJECT" tpe = Right $ Input (InputObjectType name description fields)
  where
    name = introspectionTypeName tpe
    description = introspectionTypeDescription tpe
    fields = mapOrEmpty fromMarshalledInputValue (introspectionTypeInputFields tpe)
fromMarshalledType' kind _ = Left (UnknownKind kind)

fromMarshalledFieldType :: IntrospectionField -> FieldType
fromMarshalledFieldType field = FieldType name description deprecation arguments outputTypeRef
  where
    name = introspectionFieldName field
    description = introspectionFieldDescription field
    deprecation = if isDeprecated then Deprecated deprecationReason else NotDeprecated
    isDeprecated = introspectionFieldIsDeprecated field
    deprecationReason = introspectionFieldDeprecationReason field
    arguments = fmap fromMarshalledInputValue (introspectionFieldArgs field)
    outputTypeRef = fromMarshalledTypeRef (introspectionFieldTypeRef field)

fromMarshalledInputValue :: IntrospectionInputType -> InputValue
fromMarshalledInputValue inp = InputValue name description typeRef defaultValue
  where
    name = introspectionInputTypeName inp
    description = introspectionInputTypeDescription inp
    typeRef = fromMarshalledTypeRef (introspectionInputTypeTypeRef inp)
    defaultValue = introspectionInputTypeDefaultValue inp

fromMarshalledTypeRef :: IntrospectionTypeRef -> TypeReference
fromMarshalledTypeRef (IntrospectionTypeRef "NON_NULL" _ (Just ofType)) = NonNullOf (fromMarshalledTypeRef ofType)
fromMarshalledTypeRef (IntrospectionTypeRef "LIST" _ (Just ofType)) = ListOf (fromMarshalledTypeRef ofType)
fromMarshalledTypeRef (IntrospectionTypeRef _ (Just name) _) = NamedType name
fromMarshalledTypeRef _ = UnnamedType

fromMarshalledEnumValue :: IntrospectionEnumValue -> EnumValue
fromMarshalledEnumValue enum = EnumValue name description deprecation
  where
    name = introspectionEnumValueName enum
    description = introspectionEnumValueDescription enum
    deprecation = if isDeprecated then Deprecated reason else NotDeprecated
    isDeprecated = introspectionEnumValueIsDeprecated enum
    reason = introspectionEnumValueDeprecationReason enum

mapOrEmpty :: (Functor t, Monoid (t b)) => (a -> b) -> Maybe (t a) -> t b
mapOrEmpty f = maybe mempty (fmap f)
