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
    module GraphQL.Introspection.Schema.Types
  )
where
import Relude hiding (isPrefixOf)
import qualified Data.FuzzySet as FS
import qualified Data.HashMap.Strict as Dict
import Data.Text (isPrefixOf)
import GraphQL.Introspection.Marshalling.Types
import GraphQL.Introspection.Schema.Types
import qualified Data.Vector as Vector


--- Get information about the schema

-- derefType :: TypeReference -> Schema -> Maybe GraphQLType
-- derefType (NamedType ref) schema = Dict.lookup ref (universe schema)
-- derefType (ListOf ref) schema    = derefType ref schema
-- derefType (NonNullOf ref) schema = derefType ref schema
-- derefType _ _ = Nothing

-- queryType :: Schema -> Maybe GraphQLType
-- queryType schema = derefType (query schema) schema

-- -- | fuzzy search for types
-- searchType :: Text -> Schema -> [(Double, TypeReference)]
-- searchType needle schema = map (\(score, tpeName) ->  (score, NamedType tpeName)) matches
--   where
--     matches = FS.get (fuzzTypes schema) needle

-- from marshalling data

fromMarshalledSchema :: IntrospectionSchema -> Either IntrospectionError Schema
fromMarshalledSchema schema = do
  universe <- (makeTypeDict <$> (traverse fromMarshalledType consideredTypes))
  pure (Schema queryTypeRef mutationTypeRef subscriptionTypeRef universe (typeSearchSet universe))
  where
    queryTypeRef        = NamedType <$> introspectionRootTypeName $ introspectionSchemaQueryType schema
    mutationTypeRef     = NamedType . introspectionRootTypeName <$> introspectionSchemaMutationType schema
    subscriptionTypeRef = NamedType . introspectionRootTypeName <$> introspectionSchemaSubscriptionType schema
    consideredTypes     = filter (not . (isPrefixOf "__") . introspectionTypeName) (introspectionSchemaTypes schema)
    typeSearchSet types = FS.fromList (Dict.keys types)
    makeTypeDict        = Dict.fromList (map byName universe)
    byName e            = ((name e), e)

fromMarshalledType :: IntrospectionType -> Either IntrospectionError GraphQLType
fromMarshalledType tpe = fromMarshalledType' (introspectionTypeKind tpe) tpe

fromMarshalledType' :: Text -> IntrospectionType -> Either IntrospectionError GraphQLType
fromMarshalledType' "SCALAR" tpe = Right $ Scalar (ScalarType name description)
  where
    name        = introspectionTypeName tpe
    description = introspectionTypeDescription tpe

fromMarshalledType' "OBJECT" tpe = Right $ Object (ObjectType name description fields interfaces)
  where
    name        = introspectionTypeName tpe
    description = introspectionTypeDescription tpe
    fields      = fromMarshalledFields (introspectionTypeFields tpe) 
    interfaces  = fromMarshalledTypeRefs (introspectionTypeInterfaces tpe)
      

fromMarshalledType' "INTERFACE" tpe = Right $ Interface (InterfaceType name description fields possibleTypes)
  where
    name          = introspectionTypeName tpe
    description   = introspectionTypeDescription tpe
    fields        = mapOrEmpty fromMarshalledFieldType (introspectionTypeFields tpe) 
    possibleTypes = mapOrEmpty fromMarshalledTypeRef (introspectionTypePossibleTypes tpe)
   

fromMarshalledType' "UNION" tpe = Right $ Union (UnionType name description possibleTypes)
  where
    name          = introspectionTypeName tpe
    description   = introspectionTypeDescription tpe
    possibleTypes = mapOrEmpty fromMarshalledTypeRef (introspectionTypePossibleTypes tpe)

fromMarshalledType' "ENUM" tpe = Right $ Enum (EnumType name description variants)
  where
    name        = introspectionTypeName tpe
    description = introspectionTypeDescription tpe
    variants    = mapOrEmpty fromMarshalledEnumValue (introspectionTypeEnumValues tpe)

fromMarshalledType' "INPUT_OBJECT" tpe = Right $ Input (InputObjectType name description fields)
  where
    name        = introspectionTypeName tpe
    description = introspectionTypeDescription tpe
    fields      = mapOrEmpty fromMarshalledInputValue (introspectionTypeInputFields tpe)

fromMarshalledType' kind _ = Left (IntrospectionError ("Unexpected Kind for GraphQL Type: " <> kind))

fromMarshalledFieldType :: IntrospectionField -> FieldType
fromMarshalledFieldType field = FieldType name description deprecation arguments outputTypeRef
  where
    name              = introspectionFieldName field
    description       = introspectionFieldDescription field
    deprecation       = if isDeprecated then (Deprecated deprecationReason) else NotDeprecated
    isDeprecated      = introspectionFieldIsDeprecated field
    deprecationReason = introspectionFieldDeprecationReason field
    arguments         = mapOrEmpty fromMarshalledInputValue (introspectionFieldArgs field)
    outputTypeRef     = fromMarshalledTypeRef (introspectionFieldTypeRef field)
  
fromMarshalledInputValue :: IntrospectionInputType -> InputValue
fromMarshalledInputValue inp = InputValue name description typeRef defaultValue
  where
    name         = introspectionInputTypeName inp
    description  = introspectionInputTypeDescription inp
    typeRef      = fromMarshalledTypeRef (introspectionInputTypeTypeRef inp)
    defaultValue = introspectionInputTypeDefaultValue inp

fromMarshalledTypeRef :: IntrospectionTypeRef -> TypeReference
fromMarshalledTypeRef (IntrospectionTypeRef "NON_NULL" _ ofType) = NonNullOf (fromMarshalledTypeRef ofType)
fromMarshalledTypeRef (IntrospectionTypeRef "LIST" _ ofType)     = ListOf (fromMarshalledTypeRef ofType)
fromMarshalledTypeRef (IntrospectionTypeRef _ (Just name) _)     = NamedType name 
fromMarshalledTypeRef _                                          = UnnamedType

fromMarshalledEnumValue :: IntrospectionEnumValue -> EnumValue
fromMarshalledEnumValue enum = EnumValue name description deprecation
  where
    name              = introspectionEnumValueName enum
    description       = introspectionEnumValueDescription enum
    deprecation       = if isDeprecated then (Deprecated deprecationReason) else NotDeprecated
    isDeprecated      = introspectionEnumValueIsDeprecated enum
    deprecationReason = introspectionEnumValueDeprecationReason enum

mapOrEmpty :: (Functor t) => (a -> b) -> Maybe (t a) -> t b
mapOrEmpty f v = fromMaybe mempty (fmap f <$> v)
