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
    makeTypeDict        = undefined

fromMarshalledType :: IntrospectionType -> Either IntrospectionError GraphQLType
fromMarshalledType tpe = fromMarshalledType' (introspectionTypeKind tpe) tpe

fromMarshalledType' :: Text -> IntrospectionType -> Either IntrospectionError GraphQLType

fromMarshalledType' "SCALAR" tpe = Right $ Scalar (ScalarType name description)
  where
    name        = (introspectionTypeName tpe)
    description = (introspectionTypeDescription tpe)

{-
makeSchema :: Marshalled.IntrospectionResponse -> Either IntrospectionError Schema
makeSchema resp = do
  allTypes <- (makeTypeMap <$> (traverse makeType consideredTypes))
  pure (Schema queryTypeRef mutationTypeRef subscriptionTypeRef allTypes (FS.fromList (M.keys allTypes)))
  where
    queryTypeRef        = NamedType (I.irName . I.queryType $ schema)
    mutationTypeRef     = NamedType <$> I.irName <$> (I.mutationType  schema)
    subscriptionTypeRef = NamedType <$> I.irName <$> (I.subscriptionType schema)
    schema              = I.schema resp
    consideredTypes     = filterTypes (I.types schema)

filterTypes :: [I.Type] -> [I.Type]
filterTypes = filter considerForIntrospection
  where
    considerForIntrospection tpe = not . isPrefixOf "__" $ I.itpeName  tpe

-- | TODO: ignore all types that start with __
makeType :: I.Type -> Either IntrospectionError GraphQLType 
makeType tpe = case I.itpeKind tpe of
  "SCALAR"       -> Right $ makeScalarType tpe
  "OBJECT"       -> Right $ makeObjectType tpe
  "INTERFACE"    -> Right $ makeInterfaceType tpe
  "UNION"        -> Right $ makeUnionType tpe
  "ENUM"         -> Right $ makeEnumType tpe
  "INPUT_OBJECT" -> Right $ makeInputObject tpe
  _              -> Left (IntrospectionError "Unknown output type")
  
makeScalarType :: I.Type -> Either InputType OutputType
makeScalarType I.Type { I.itpeName = typeName, I.itpeDescription = description, .. } = (Right (ScalarOutputType (ScalarType typeName description)))

makeObjectType :: I.Type -> Either InputType OutputType
makeObjectType I.Type { I.itpeName = typeName, I.itpeDescription = description, I.itpeFields = fields, I.itpeInterfaces = interfaces, .. } = (Right (ObjectOututType (ObjectType typeName description fieldTypes interfaceTypes)))
  where
    fieldTypes     =  fromMaybe [] ((map makeFieldType) <$> fields)
    interfaceTypes =  fromMaybe [] ((map makeTypeRef) <$> interfaces)

makeInterfaceType :: I.Type -> Either InputType OutputType
makeInterfaceType I.Type { I.itpeName = typeName, I.itpeDescription = description, I.itpeFields = fields, I.itpePossibleTypes = possibleTypes, .. } = (Right (InterfaceOutputType (InterfaceType typeName description fieldTypes possibleTypeRefs)))
  where
    fieldTypes       = fromMaybe [] ((map makeFieldType) <$> fields)
    possibleTypeRefs = fromMaybe [] ((map makeTypeRef) <$> possibleTypes)

makeUnionType :: I.Type -> Either InputType OutputType
makeUnionType I.Type { I.itpeName = typeName, I.itpeDescription = description, I.itpePossibleTypes = possibleTypes, .. } = (Right (UnionOutputType (UnionType typeName description possibleTypeRefs)))
  where
    possibleTypeRefs = fromMaybe [] ((map makeTypeRef) <$> possibleTypes)

makeEnumType :: I.Type -> Either InputType OutputType
makeEnumType I.Type { I.itpeName = typeName, I.itpeDescription = description, I.itpeEnumValues = enums, .. } = (Right (EnumOutputType (EnumType typeName description values)))
  where
    values = fromMaybe [] ((map makeEnumValue) <$> enums)

makeEnumValue :: I.EnumValues -> EnumValue
makeEnumValue I.EnumValues { I.ievName = enumName, I.ievDescription = description, I.ievIsDeprecated = False, .. } = EnumValue enumName description False Nothing 
makeEnumValue I.EnumValues { I.ievName = enumName, I.ievDescription = description, I.ievIsDeprecated = True, I.ievDeprecationReason = reason } = EnumValue enumName description True reason

makeInputObject :: I.Type -> Either InputType OutputType
makeInputObject I.Type { I.itpeName = typeName, I.itpeDescription = description, I.itpeInputFields = fields, .. } = (Left (ObjectInputType (InputObjectType typeName description fieldTypes)))
  where
    fieldTypes = fromMaybe [] ((map makeInputValue) <$> fields)

makeInputValue :: I.InputType -> InputValue
makeInputValue I.InputType { I.iiName = tpeName, I.iiDescription = description, I.iiTypeRef = tpeRef, I.iiDefaultValue = defaultValue } = (InputValue tpeName description (makeTypeRef tpeRef) defaultValue)

makeTypeRef :: I.TypeRef -> TypeReference
makeTypeRef I.TypeRef { I.itrKind = "NON_NULL", I.itrOfType = (Just inner), .. } = NonNullOf (makeTypeRef inner) 
makeTypeRef I.TypeRef { I.itrKind = "LIST", I.itrOfType = (Just inner), .. }     = ListOf (makeTypeRef inner)
makeTypeRef I.TypeRef { I.itrName = (Just tpeName), .. }                         = NamedType tpeName
makeTypeRef _                                                                    = UnnamedType

makeFieldType :: I.Field -> FieldType
makeFieldType I.Field { I.ifName = fieldName, I.ifDescription = description, I.ifIsDeprecated = isDeprecated, I.ifDeprecationReason = deprecationReason, I.ifTypeRef = typeRef, I.ifArgs = args } = FieldType fieldName description isDeprecated deprecationReason (makeTypeRef typeRef) inputArgs
  where
    inputArgs = (map makeInputValue) args

makeTypeMap :: [(Either InputType OutputType)] ->  M.HashMap Text (Either InputType OutputType)
makeTypeMap = foldl' insertInfo M.empty
  where
    insertInfo hashMap (Left tpe)  = M.insert (name tpe) (Left tpe) hashMap
    insertInfo hashMap (Right tpe) = M.insert (name tpe) (Right tpe) hashMap

-}
