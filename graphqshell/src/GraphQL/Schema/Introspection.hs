{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GraphQL.Schema.Introspection (
    schemaFromIntrospectionResponse
  , Schema
  , IntrospectionError(..)
  , HasDeprecation
  , isDeprecated
  , deprecationReason
  , HasDescription
  , description
  , HasName
  , name
  , GraphQLType
  , GraphQLValue
  , ObjectType(..)
  , ObjectLikeType
  , InputObjectType(..)
  , InputValue(..)
  , ScalarType(..)
  , AbstractType
  , possibleTypes
  , UnionType(..)
  , InterfaceType(..)
  , Directive(..)
  , EnumValue
  , Field
  , I.introspectionQuery
  ) where
import Relude
import Data.Either.Combinators (mapLeft)
import Data.Aeson (eitherDecode)
import qualified GraphQL.Schema.Introspection.Internal as I

data IntrospectionError = IntrospectionError String deriving (Eq, Show, Exception)

newtype Schema = Schema {
  internal :: I.IntrospectionSchema
} deriving (Eq, Show)

class GraphQLType a
class GraphQLValue a

class HasName a where
  name :: a -> String
  
class HasDescription a where
  description :: a -> Maybe String

class HasDeprecation a where
  isDeprecated :: a -> Bool
  deprecationReason :: a -> Maybe String

data WrapperType a = ListOf a | NonNull a deriving (Eq, Show, Generic)

class (GraphQLType a) => AbstractType a where
  possibleTypes :: (GraphQLType b) => a -> [b]

class ObjectLikeType a

data ObjectType = ObjectType {
    otName :: String 
  , otDescription :: Maybe String
  , otFields :: [Field]
  , otInterfaces :: [InterfaceType]
} deriving (Eq, Show, Generic, ObjectLikeType)

instance HasName ObjectType where
  name = otName

instance HasDescription ObjectType where
  description = otDescription

data InputObjectType = InputObjectType {
    itName :: String
  , itDescription :: Maybe String
  , itFields :: [InputValue]
} deriving (Eq, Show, Generic)

instance HasName InputObjectType where
  name = itName

instance HasDescription InputObjectType where
  description = itDescription

data InterfaceType = InterfaceType {
    ifName :: String
  , ifDescription :: Maybe String
  , ifPossibleTypes :: [ObjectType]
  , ifFields :: [Field]
} deriving (Eq, Show, Generic, ObjectLikeType)

instance HasName InterfaceType where
  name = ifName

instance HasDescription InterfaceType where
  description = ifDescription
  
data UnionType = UnionType {
    utName :: String
  , utDescription :: Maybe String
  , utPossibleTypes :: [ObjectType]
} deriving (Eq, Show, Generic, ObjectLikeType)

instance HasName UnionType where
  name = utName

instance HasDescription UnionType where
  description = utDescription


data ScalarType = ScalarType {
    stName :: String
  , stDescription :: Maybe String
} deriving (Eq, Show, Generic)

instance HasName ScalarType where
  name = stName

instance HasDescription ScalarType where
  description = stDescription

data EnumType = EnumType {
    etName :: String
  , etDescription :: Maybe String
  , etValues :: [EnumValue]
} deriving (Eq, Show, Generic)

instance HasName EnumType where
  name = etName

instance HasDescription EnumType where
  description = etDescription

data Field = Field {
    fName :: String
  , fDescription :: Maybe String
  , fArgs :: [InputValue]
  , fIsDeprecated :: Bool
  , fDeprecationReason :: Maybe String
} deriving (Eq, Show, Generic)

instance HasName Field where
  name = fName

instance HasDescription Field where
  description = fDescription

instance HasDeprecation Field where
  isDeprecated = fIsDeprecated
  deprecationReason = fDeprecationReason


data InputValue = InputValue {
    ivName :: String
  , ivDescription :: Maybe String
  , ivType :: Either ScalarType InputObjectType 
  , ivDefaultValue :: String
} deriving (Eq, Show, Generic)

instance GraphQLValue InputValue

instance HasName InputValue where
  name = ivName

instance HasDescription InputValue where
  description = ivDescription

data EnumValue = EnumValue {
    evName :: String
  , evDescription :: Maybe String
  , evIsDeprecated :: Bool
  , evDeprecationReason :: Maybe String
} deriving (Eq, Show, Generic)

instance GraphQLValue EnumValue

instance HasName EnumValue where
  name = evName

instance HasDescription EnumValue where
  description = evDescription

instance HasDeprecation EnumValue where
  isDeprecated = evIsDeprecated
  deprecationReason = evDeprecationReason

data Directive = Directive {
    dName :: String
  , dDescription :: Maybe String
  , dlocations :: [String]
  , dArgs :: [InputValue]
} deriving (Eq, Show, Generic)

schemaFromIntrospectionResponse :: LByteString -> Either IntrospectionError Schema
schemaFromIntrospectionResponse jsonResponse = makeSchema <$> parseResponse 
 where
   parseResponse :: Either IntrospectionError I.IntrospectionResponse
   parseResponse = mapLeft IntrospectionError (eitherDecode jsonResponse)

makeSchema :: I.IntrospectionResponse -> Schema
makeSchema = undefined
