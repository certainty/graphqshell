{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecordWildCards #-}

module GraphQL.Introspection.Schema.Types (
    schemaFromIntrospectionResponse
  , makeSchema
  , Schema(..)
  , IntrospectionError(..)
  , InputValue(..)
  , ScalarType(..)
  , UnionType(..)
  , InterfaceType(..)
  , EnumValue
  , TypeReference(..)
  , GraphQLType
  , I.introspectionQuery
  ) where
import Relude hiding (length, drop, isPrefixOf)
import qualified Data.FuzzySet as FS
import qualified Data.HashMap.Strict as M
import Data.Text (isPrefixOf)
import qualified GraphQL.Introspection.Marshalling.Types as I
import GraphQL.Client.Types (GraphQLResponse(..), GraphQLError)
import Data.Vector

data IntrospectionError = IntrospectionError Text
      | PartialResult [GraphQLError]
      deriving (Eq, Show, Exception)

-- Schema
type TypeUniverse = M.HashMap Text Type

data Schema = Schema
  {   query        :: TypeReference
    , mutation     :: Maybe TypeReference
    , subscription :: Maybe TypeReference
    , universe     :: TypeUniverse 
                   -- ^ The type universe of the schema
    , fuzzTypes    :: FS.FuzzySet
                  -- ^ Fuzzy index of fields
  } deriving (Eq, Show)

-- Type wrapper
data GraphQLType = Scalar ScalarType
                 | Object ObjectType
                 | Input InputObjectType
                 | Enum EnumType
                 | Interface InterfaceType
                 | Union UnionType
                 deriving (Eq, Show)

-- Individual types
type Name                = Text
type Description         = Maybe Text
type Fields              = Vector FieldType
type InputFields         = Vector InputValue
type Interfaces          = Vector TypeReference
type PossibleTypes       = Vector TypeReference
type EnumVariants        = Vector EnumValue
type Arguments           = Vector InputValue
type DeprecationFlag     = Bool 
type DeprecationReason   = Maybe Text
type OutputTypeReference = TypeReference
type InputTypeReference  = TypeReference
type DefaultValue        = Maybe Text

data ScalarType      = ScalarType Name Description deriving (Eq,Show)
data ObjectType      = ObjectType Name Description Fields Interfaces deriving (Eq, Show)
data UnionType       = UnionType Name Description PossibleTypes deriving (Eq, Show)
data InterfaceType   = InterfaceType Name Description Fields PossibleTypes deriving (Eq, Show)
data EnumType        = EnumType Name Description EnumVariants deriving (Eq, Show)
data InputObjectType = InputObjectType Name Description InputFields deriving (Eq, Show)
data FieldType       = FieldType Name Description DeprecationFlag DeprecationReason Arguments OutputTypeReference deriving (Eq, Show)
data EnumValue       = EnumValue Name Description DeprecationFlag DeprecationReason deriving (Eq, Show)
data InputValue      = InputValue Name Description InputTypeReference DefaultValue  deriving (Eq, Show)
data TypeReference   = ListOf TypeReference
                     | NonNullOf TypeReference
                     | NamedType Name
                     | UnnamedType
                     deriving (Eq, Show)

-- Common functionality is exposed via type classes

class HasName a where
  name :: a -> Text

instance HasName ScalarType where
  name (ScalarType n _) = n 

instance HasName ObjectType where
  name (ObjectType n _ _ _) = n

instance HasName UnionType where
  name (UnionType n _ _ ) = n

instance HasName InterfaceType where
  name (InterfaceType n _ _ _)= n

instance HasName EnumType where
  name (EnumType n _ _) = n

instance HasName InputObjectType where
  name (InputObjectType n _ _) = n

instance HasName FieldType where
  name (FieldType n _ _ _ _ _) = n

instance  HasName GraphQLType where
  name (Scalar t) = name t
  name (Object t) = name t
  name (Enum t) = name t
  name (Interface t) = name t
  name (Union t) = name t
  name (Input t) = name t

instance (HasName a, HasName b) => HasName (Either a b) where
  name (Right t) = name t
  name (Left t) = name t

-- TODO: move to GraphQL.Introspection
schemaFromIntrospectionResponse :: GraphQLResponse I.IntrospectionResponse -> Either IntrospectionError Schema
schemaFromIntrospectionResponse (GraphQLResponse (Just resp) Nothing) = makeSchema resp
schemaFromIntrospectionResponse (GraphQLResponse _ (Just errors))     = Left (PartialResult errors) 
schemaFromIntrospectionResponse (GraphQLResponse _ _)                 = Left (IntrospectionError "Empty Result")  -- TODO: wrap GraphQLError

makeSchema :: I.IntrospectionResponse -> Either IntrospectionError Schema
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

