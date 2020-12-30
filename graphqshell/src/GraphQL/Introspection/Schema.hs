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
    fromIntrospectionSchema,
    lookupType,
    searchType,
    isNullable,
    fieldTypeReference,
  )
where

import Control.Exception.Safe (MonadThrow, throw)
import qualified Data.HashMap.Strict as Dict
import Data.Text (isPrefixOf)
import qualified Data.Vector as Vector
import GraphQL.Introspection.Marshalling.Types
  ( IntrospectionEnumValue (introspectionEnumValueName),
    introspectionEnumValueDeprecationReason,
    introspectionEnumValueDescription,
    introspectionEnumValueIsDeprecated,
    introspectionFieldArgs,
    introspectionFieldDeprecationReason,
    introspectionFieldDescription,
    introspectionFieldIsDeprecated,
    introspectionFieldName,
    introspectionFieldTypeRef,
    introspectionInputTypeDefaultValue,
    introspectionInputTypeDescription,
    introspectionInputTypeName,
    introspectionInputTypeTypeRef,
    introspectionSchemaMutationType,
    introspectionSchemaQueryType,
    introspectionSchemaSubscriptionType,
    introspectionSchemaTypes,
    introspectionTypeDescription,
    introspectionTypeEnumValues,
    introspectionTypeFields,
    introspectionTypeInputFields,
    introspectionTypeInterfaces,
    introspectionTypeKind,
    introspectionTypeName,
    introspectionTypePossibleTypes,
  )
import qualified GraphQL.Introspection.Marshalling.Types as MTypes
import GraphQL.Introspection.Schema.Types hiding (deprecationReason, description, isDeprecated, name)
import qualified GraphQL.Introspection.Schema.Types as Types
import Relude hiding (isPrefixOf)
import qualified Text.Fuzzy as Fz
import Utils

data SchemaBuildError
  = MissingQueryType
  | InvalidRootType Text
  | UnexpectedType Text
  | InvalidType Text
  deriving (Eq, Show)

instance Exception SchemaBuildError

type TypeUniverse = Dict.HashMap Text GraphQLType

type QueryType = ObjectType

type MutationType = Maybe ObjectType

type SubscriptionType = Maybe ObjectType

data Schema = Schema
  { query :: QueryType,
    mutation :: MutationType,
    subscription :: SubscriptionType,
    -- | The type universe of the schema
    universe :: TypeUniverse,
    -- | Fuzzy index of type names
    typeNames :: [Text]
  }
  deriving (Eq, Show)

instance Inspect Schema where
  inspect _ = "Schema { .. }"

mkSchema :: QueryType -> MutationType -> SubscriptionType -> [GraphQLType] -> Schema
mkSchema queryType mutationType subscriptionType additionalTypes = Schema queryType mutationType subscriptionType typeUniverse typeIndex
  where
    typeIndex = Dict.keys typeUniverse
    typeUniverse = Dict.fromList (map (\tpe -> (Types.name tpe, tpe)) additionalTypes)

--- Get information about the schema
lookupType :: TypeReference -> Schema -> Maybe GraphQLType
lookupType (Named (NamedType ref)) schema = Dict.lookup ref (universe schema)
lookupType (ListOf ref) schema = lookupType ref schema
lookupType (NonNullOf ref) schema = lookupType ref schema

-- -- | fuzzy search for types in the schema provided
searchType ::
  -- | The text to search for
  Text ->
  -- | A pair specifying the prefix and suffix to surround the matches
  (Text, Text) ->
  -- | The schema
  Schema ->
  -- | A list of matches containing the score, the annotated match and the reference to the type
  [(NamedType, Text, Int)]
searchType needle (prefix, suffix) schema = map wrapMatch matches
  where
    matches = Fz.filter needle (typeNames schema) prefix suffix identity False
    wrapMatch match = (NamedType $ Fz.original match, Fz.rendered match, Fz.score match)

isNullable :: TypeReference -> Bool
isNullable (NonNullOf _) = False
isNullable _ = True

fieldTypeReference :: FieldType -> TypeReference
fieldTypeReference (FieldType _ _ _ _ tpe) = tpe

fromIntrospectionSchema :: (MonadThrow m) => MTypes.IntrospectionSchema -> m Schema
fromIntrospectionSchema schema = mkSchema <$> queryType <*> mutationType <*> subscriptionType <*> otherTypes
  where
    queryType = objectTypeFromIntrospectionType (introspectionSchemaQueryType schema)
    mutationType = sequence $ objectTypeFromIntrospectionType <$> introspectionSchemaMutationType schema
    subscriptionType = sequence $ objectTypeFromIntrospectionType <$> introspectionSchemaSubscriptionType schema
    otherTypes = traverse fromIntrospectionType (filterReservedTypes (introspectionSchemaTypes schema))
    filterReservedTypes tpes = filter (not . isPrefixOf "__" . introspectionTypeName) (Vector.toList tpes)

objectTypeFromIntrospectionType :: (MonadThrow m) => MTypes.IntrospectionType -> m ObjectType
objectTypeFromIntrospectionType introType = do
  tpe <- fromIntrospectionType introType
  case tpe of
    (Object o) -> pure o
    _ -> throw (UnexpectedType "Excepected ObjectType")

fromIntrospectionType :: (MonadThrow m) => MTypes.IntrospectionType -> m GraphQLType
fromIntrospectionType tpe = fromIntrospectionType' (introspectionTypeKind tpe) tpe

fromIntrospectionType' :: (MonadThrow m) => MTypes.IntrospectionTypeKind -> MTypes.IntrospectionType -> m GraphQLType
fromIntrospectionType' MTypes.Scalar tpe = pure $ Scalar $ ScalarType tpeName tpeDescription
  where
    tpeName = introspectionTypeName tpe
    tpeDescription = introspectionTypeDescription tpe
fromIntrospectionType' MTypes.Object tpe = Object <$> (ObjectType <$> tpeName <*> tpeDescription <*> tpeFields <*> tpeInterfaces)
  where
    tpeName = pure $ introspectionTypeName tpe
    tpeDescription = pure $ introspectionTypeDescription tpe
    tpeFields = traverse fromIntrospectionFieldType (maybeToVec (introspectionTypeFields tpe))
    tpeInterfaces = traverse (guardNonWrapperType NamedType) (maybeToVec (introspectionTypeInterfaces tpe))
fromIntrospectionType' MTypes.Union tpe = Union <$> (UnionType <$> tpeName <*> tpeDescription <*> tpePossibleTypes)
  where
    tpeName = pure $ introspectionTypeName tpe
    tpeDescription = pure $ introspectionTypeDescription tpe
    tpePossibleTypes = traverse (guardNonWrapperType NamedType) (maybeToVec (introspectionTypePossibleTypes tpe))
fromIntrospectionType' MTypes.Interface tpe = Interface <$> (InterfaceType <$> tpeName <*> tpeDescription <*> tpeFields <*> tpePossibleTypes)
  where
    tpeName = pure $ introspectionTypeName tpe
    tpeDescription = pure $ introspectionTypeDescription tpe
    tpeFields = traverse fromIntrospectionFieldType (maybeToVec (introspectionTypeFields tpe))
    tpePossibleTypes = traverse (guardNonWrapperType NamedType) (maybeToVec (introspectionTypePossibleTypes tpe))
fromIntrospectionType' MTypes.Enum tpe = pure $ Enum $ EnumType tpeName tpeDescription tpeVariants
  where
    tpeName = introspectionTypeName tpe
    tpeDescription = introspectionTypeDescription tpe
    tpeVariants = fromIntrospectionEnumValue <$> maybeToVec (introspectionTypeEnumValues tpe)
fromIntrospectionType' MTypes.InputObject tpe = Input <$> (InputObjectType <$> tpeName <*> tpeDescription <*> tpeFields)
  where
    tpeName = pure $ introspectionTypeName tpe
    tpeDescription = pure $ introspectionTypeDescription tpe
    tpeFields = traverse fromIntrospectionInputValue (maybeToVec (introspectionTypeInputFields tpe))
fromIntrospectionType' kind _ = throw (UnexpectedType ("Unexpected kind " <> show kind))

fromIntrospectionFieldType :: (MonadThrow m) => MTypes.IntrospectionField -> m FieldType
fromIntrospectionFieldType field = FieldType <$> fName <*> fDescription <*> fDeprecation <*> fArguments <*> fType
  where
    fName = pure $ introspectionFieldName field
    fDescription = pure $ introspectionFieldDescription field
    fDeprecation = pure $ if isDeprecated then Deprecated deprecationReason else NotDeprecated
    isDeprecated = introspectionFieldIsDeprecated field
    deprecationReason = introspectionFieldDeprecationReason field
    fArguments = traverse fromIntrospectionInputValue (introspectionFieldArgs field)
    fType = fromIntrospectionTypeRef (introspectionFieldTypeRef field)

fromIntrospectionInputValue :: (MonadThrow m) => MTypes.IntrospectionInputType -> m InputValue
fromIntrospectionInputValue inp = InputValue <$> inpName <*> inpDescription <*> typeRef <*> defaultValue
  where
    inpName = pure $ introspectionInputTypeName inp
    inpDescription = pure $ introspectionInputTypeDescription inp
    typeRef = fromIntrospectionTypeRef (introspectionInputTypeTypeRef inp)
    defaultValue = pure $ introspectionInputTypeDefaultValue inp

fromIntrospectionTypeRef :: (MonadThrow m) => MTypes.IntrospectionTypeRef -> m TypeReference
fromIntrospectionTypeRef (MTypes.IntrospectionTypeRef MTypes.NonNull _ (Just ofType)) = NonNullOf <$> fromIntrospectionTypeRef ofType
fromIntrospectionTypeRef (MTypes.IntrospectionTypeRef MTypes.List _ (Just ofType)) = ListOf <$> fromIntrospectionTypeRef ofType
fromIntrospectionTypeRef (MTypes.IntrospectionTypeRef _ (Just name) _) = pure $ Named (NamedType name)
fromIntrospectionTypeRef MTypes.IntrospectionTypeRef {} = throw (InvalidType "Named type reference without name")

fromIntrospectionEnumValue :: MTypes.IntrospectionEnumValue -> EnumValue
fromIntrospectionEnumValue enum = EnumValue enName enDescription enDeprecation
  where
    enName = introspectionEnumValueName enum
    enDescription = introspectionEnumValueDescription enum
    enDeprecation = if isDeprecated then Deprecated reason else NotDeprecated
    isDeprecated = introspectionEnumValueIsDeprecated enum
    reason = introspectionEnumValueDeprecationReason enum

guardNonWrapperType :: (MonadThrow m) => (Text -> b) -> MTypes.IntrospectionTypeRef -> m b
guardNonWrapperType _ (MTypes.IntrospectionTypeRef MTypes.NonNull _ _) = throw (UnexpectedType "NON_NULL")
guardNonWrapperType _ (MTypes.IntrospectionTypeRef MTypes.List _ _) = throw (UnexpectedType "LIST")
guardNonWrapperType f (MTypes.IntrospectionTypeRef _ (Just name) _) = pure (f name)
guardNonWrapperType _ _ = throw (InvalidType "Named type reference without name")

maybeToVec :: Maybe (Vector.Vector a) -> Vector.Vector a
maybeToVec (Just v) = v
maybeToVec Nothing = Vector.empty
