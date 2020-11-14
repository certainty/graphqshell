{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecordWildCards #-}

module GraphQL.Schema.Introspection (
    schemaFromIntrospectionResponse
  , Schema
  , IntrospectionError(..)
  , InputValue(..)
  , ScalarType(..)
  , UnionType(..)
  , InterfaceType(..)
  , EnumValue
  , TypeReference(..)
  , I.introspectionQuery
  , derefType
  , queryType
  ) where
import Relude hiding (length, drop)
import Data.Either.Combinators (mapLeft)
import Data.Aeson (eitherDecode)
import qualified Data.FuzzySet as FS
import qualified Data.HashMap.Strict as M
import qualified GraphQL.Schema.Introspection.Internal as I

data IntrospectionError = IntrospectionError Text deriving (Eq, Show, Exception)

type GraphQLType = Either InputType OutputType

data Schema = Schema
  { query        :: TypeReference,
    mutation     :: Maybe TypeReference,
    subscription :: Maybe TypeReference,
    universe     :: M.HashMap Text GraphQLType,
    fuzzTypes    :: FS.FuzzySet
                 -- ^ The type universe of the schema
  } deriving (Eq, Show)

data TypeInformation a = TypeInformation a deriving (Eq, Show)
  
data ScalarType      = ScalarType
  { stName :: Text, stDescription :: Maybe Text }
  deriving (Eq,Show)

data ObjectType      = ObjectType
  { otName :: Text, otDescription :: Maybe Text, otFields :: [FieldType], otInterfaces :: [TypeReference] }
  deriving (Eq, Show)

data UnionType       = UnionType
  { utName :: Text, utDescription :: Maybe Text, utPossibleTypes :: [TypeReference] }
  deriving (Eq, Show)

data InterfaceType   = InterfaceType
  { itName :: Text, itDescription :: Maybe Text, itFields :: [FieldType], itPossibleTypes :: [TypeReference] }
  deriving (Eq, Show)

data EnumType        = EnumType
  { etName :: Text, etDescription :: Maybe Text, etValues :: [EnumValue] }
  deriving (Eq, Show)

data InputObjectType = InputObjectType
  { ioName :: Text, ioDescription :: Maybe Text, ioFields :: [InputValue] }
  deriving (Eq, Show)

data FieldType       = FieldType
  { ftName :: Text, ftDescription :: Maybe Text, ftIsDeprecated :: Bool, ftDeprecationReason :: Maybe Text, ftType :: TypeReference, ftArgs :: [InputValue] }
  deriving (Eq, Show)

data OutputType = ScalarOutputType ScalarType
                | ObjectOututType ObjectType
                | EnumOutputType EnumType
                | InterfaceOutputType InterfaceType
                | UnionOutputType UnionType
                | NonNullOuputType OutputType
                | ListOutputType OutputType
                deriving (Eq, Show)

data InputType = ObjectInputType InputObjectType
               | ScalarInputType ScalarType
               | EnumInputType EnumType
               deriving (Eq, Show)

data TypeReference = ListOf TypeReference
                   | NonNullOf TypeReference
                   | NamedType Text
                   | UnnamedType
                   deriving (Eq, Show)

data InputValue = InputValue
  { ivName :: Text, ivDescription :: Maybe Text, ivType :: TypeReference, ivDefaultValue :: Maybe Text }
  deriving (Eq, Show)

data EnumValue  = EnumValue
  { evName :: Text, evDescription :: Maybe Text, evIsDeprecated :: Bool, evDeprecationReason :: Maybe Text }
  deriving (Eq, Show)

class HasName a where
  name :: a -> Text

instance HasName ScalarType where
  name = stName

instance HasName ObjectType where
  name = otName

instance HasName UnionType where
  name = utName

instance HasName InterfaceType where
  name = itName

instance HasName EnumType where
  name = etName

instance HasName InputObjectType where
  name = ioName

instance HasName FieldType where
  name = ftName

instance  HasName OutputType where
  name (ScalarOutputType t) = name t
  name (ObjectOututType t) = name t
  name (EnumOutputType t) = name t
  name (InterfaceOutputType t) = name t
  name (UnionOutputType t) = name t
  name (NonNullOuputType t) = name t
  name (ListOutputType t) = name t

instance HasName InputType where
  name (ObjectInputType t) = name t
  name (ScalarInputType t) = name t
  name (EnumInputType t) = name t

instance (HasName a, HasName b) => HasName (Either a b) where
  name (Right t) = name t
  name (Left t) = name t

schemaFromIntrospectionResponse :: LByteString -> Either IntrospectionError Schema
schemaFromIntrospectionResponse jsonResponse = parseResponse >>= makeSchema
 where
   parseResponse :: Either IntrospectionError I.IntrospectionResponse
   parseResponse = mapLeft (IntrospectionError . toText) (eitherDecode jsonResponse)

makeSchema :: I.IntrospectionResponse -> Either IntrospectionError Schema
makeSchema resp = do
  allTypes <- (makeTypeMap <$> (traverse makeType (I.types schema)))
  pure (Schema queryTypeRef mutationTypeRef subscriptionTypeRef allTypes (FS.fromList (M.keys allTypes)))
  where
    queryTypeRef        = NamedType (I.irName . I.queryType $ schema)
    mutationTypeRef     = NamedType <$> I.irName <$> (I.mutationType  schema)
    subscriptionTypeRef = NamedType <$> I.irName <$> (I.subscriptionType schema)
    schema              = I.schema resp

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



--- Get information about the schema

derefType :: TypeReference -> Schema -> Maybe GraphQLType
derefType (NamedType ref) schema = M.lookup ref (universe schema)
derefType (ListOf ref) schema = derefType ref schema
derefType (NonNullOf ref) schema = derefType ref schema
derefType _ _ = Nothing

queryType :: Schema -> Maybe GraphQLType
queryType schema = derefType (query schema) schema
