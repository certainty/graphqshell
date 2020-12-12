{-# LANGUAGE TemplateHaskell #-}

module Shell.Configuration.Types where
import Relude
import Text.URI (URI,mkURI)
import Data.Yaml (withObject, withArray)
import Data.Yaml.Aeson (FromJSON(..), (.:),(.:?),(.!=), Parser, Value)
import Data.Vector hiding (sequence)
import Lens.Micro.Platform (makeLenses)

data ApplicationConfig = ApplicationConfig {
  _appConfigEndpoints :: [EndpointConfig],
  _appConfigThemes :: [ThemeConfig]
} deriving (Generic, Eq)


data EndpointConfig = EndpointConfig {
  _endpointName :: Text,
  _endpointIsDefault :: Bool,
  _endpointURL :: URI,
  _endpointLink :: Maybe URI,
  _endpointHttpConfig :: Maybe EndpointHttpConfig
} deriving (Generic, Show, Eq)
        
data EndpointHttpConfig = EndpointHttpConfig {
  _endpointHttpHeaders :: Maybe (Vector (Text, Text))
} deriving (Generic, Show, Eq)

data ThemeConfig = ThemeConfig {
  _themeName :: Text,
  _themeIsDefault :: Bool,
  _themePath :: FilePath 
} deriving (Generic, Show, Eq)

makeLenses ''ApplicationConfig
makeLenses ''EndpointConfig
makeLenses ''EndpointHttpConfig
makeLenses ''ThemeConfig


-- Marshalling
instance FromJSON ApplicationConfig where
  parseJSON = withObject "ApplicationConfig" $ \o -> do
    ApplicationConfig
      <$> o .: "endpoints"
      <*> o .: "themes"
      
instance FromJSON EndpointConfig where
  parseJSON = withObject "EndpointConfig" $ \o -> do
    EndpointConfig
      <$> o .: "name"
      <*> o .:? "default" .!= False 
      <*> ((o .: "url") >>= parseURI)
      <*> ((o .:? "link") >>= parseMaybeURI)
      <*> o .:? "http"
    where
     parseMaybeURI u = sequence $ parseURI <$> u
     parseURI uri = case (mkURI uri) :: Maybe URI of
       (Just u) -> pure u
       _ -> fail "Could not parse URI"

instance FromJSON EndpointHttpConfig where
  parseJSON = withObject "EndpointHttpConfig" $ \o -> do
    headers <- o .:? "custom-headers"
    parsedHeaders <- sequence $ (parseCustomHeaders <$> headers)
    pure $ EndpointHttpConfig parsedHeaders
    where
      parseCustomHeaders :: Value -> Parser (Vector (Text, Text))
      parseCustomHeaders = withArray "headers" (traverse parsePairs)
      parsePairs :: Value -> Parser (Text, Text)
      parsePairs = withObject "header" $ \o -> do
        key <- o .: "name"
        val <- o .: "value"
        pure (key, val)
        
instance FromJSON ThemeConfig where
  parseJSON = withObject "ThemeConfig" $ \o -> do
    ThemeConfig
      <$> o .: "name"
      <*> o .:? "default" .!= False
      <*> o .: "path"
    
