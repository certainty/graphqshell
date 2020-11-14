{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , I.introspectionQuery
  ) where
import Relude
import Data.Either.Combinators (mapLeft)
import Data.Aeson (eitherDecode)
import qualified Data.HashMap.Strict as M
import qualified GraphQL.Schema.Introspection.Internal as I

data IntrospectionError = IntrospectionError String deriving (Eq, Show, Exception)

data Schema = Schema
  { query        :: TypeInformation OutputType,
    mutation     :: Maybe (TypeInformation OutputType),
    subscription :: Maybe (TypeInformation OutputType),
    allTypes     :: M.HashMap String (TypeInformation (Either InputType OutputType))
  } deriving (Eq, Show)

data TypeInformation a = TypeInformation a deriving (Eq, Show)
  
data ScalarType      = ScalarType
  { stName :: String, stDescription :: Maybe String }
  deriving (Eq,Show)

data ObjectType      = ObjectType
  { otName :: String, otDescription :: Maybe String, otFields :: [FieldType], otInterfaces :: [InterfaceType] }
  deriving (Eq, Show)

data UnionType       = UnionType
  { utName :: String, utDescription :: Maybe String, utPossibleTypes :: [ObjectType] }
  deriving (Eq, Show)

data InterfaceType   = InterfaceType
  { itName :: String, itDescription :: Maybe String, itFields :: [FieldType], itPossibleTypes :: [ObjectType] }
  deriving (Eq, Show)

data EnumType        = EnumType
  { etName :: String, etDescription :: Maybe String, etValues :: [EnumValue] }
  deriving (Eq, Show)

data InputObjectType = InputObjectType
  { ioName :: String, ioDescription :: Maybe String, ioFields :: [InputValue] }
  deriving (Eq, Show)

data FieldType       = FieldType
  { ftName :: String, ftDescription :: Maybe String, ftIsDeprecated :: Bool, ftDeprecationReason :: Maybe String, ftType :: OutputType, ftArgs :: [InputType] }
  deriving (Eq, Show)

data OutputType = ScalarOutputType ScalarType
                | ObjectOututType ObjectType
                | EnumOutputType EnumType
                | InterfaceOutputType InterfaceType
                | UnionOutputType UnionType
                | NonNnullOuputType OutputType
                | ListOutputType OutputType
                deriving (Eq, Show)

data InputType = ObjectInputType InputObjectType
               | ScalarInputType ScalarType
               | EnumInputType EnumType
               deriving (Eq, Show)

data InputValue = InputObjectValue
  { ivName :: String, ivDescription :: Maybe String, ivType :: InputType, ivDefaultValue :: Maybe String }
  deriving (Eq, Show)

data EnumValue  = EnumValue
  { evName :: String, evDescription :: Maybe String, evIsDeprecated :: Bool, evDeprecationReason :: Maybe String }
  deriving (Eq, Show)

schemaFromIntrospectionResponse :: LByteString -> Either IntrospectionError Schema
schemaFromIntrospectionResponse jsonResponse = makeSchema <$> parseResponse 
 where
   parseResponse :: Either IntrospectionError I.IntrospectionResponse
   parseResponse = mapLeft IntrospectionError (eitherDecode jsonResponse)

makeSchema :: I.IntrospectionResponse -> Schema
makeSchema resp = Schema query mutation subscription allTypes 
  where
    allTypes      = M.fromList (map makeTypeInfo (I.types . I.schema $ resp))
    query         = makeRootTypeInfo ((M.!) allTypes "query")
    mutation      = makeRootTypeInfo <$> (M.lookup "mutation" allTypes)
    subscription  = makeRootTypeInfo <$> (M.lookup "subscription" allTypes)

makeTypeInfo :: I.Type -> (String, TypeInformation (Either InputType OutputType))
makeTypeInfo I.Type { I.name = "SCALAR", ..} = undefined
makeTypeInfo I.Type { I.name = "OBJECT", ..} = undefined
makeTypeInfo I.Type { I.name = "INTERFACE", ..} = undefined
makeTypeInfo I.Type { I.name = "UNION", ..} = undefined
makeTypeInfo I.Type { I.name = "ENUM", ..} = undefined
makeTypeInfo I.Type { I.name = "INPUT_OBJECT", ..} = undefined
makeTypeInfo I.Type { I.name = "LIST", ..} = undefined
makeTypeInfo I.Type { I.name = "NON_NULL", ..} = undefined

makeRootTypeInfo :: TypeInformation (Either InputType OutputType) -> TypeInformation OutputType
makeRootTypeInfo (TypeInformation (Right outputType)) = makeTypeInformation outputType
makeRootTypeInfo _ = error "Can not extract RootType"

makeTypeInformation :: a -> TypeInformation a
makeTypeInformation tpe = TypeInformation tpe
