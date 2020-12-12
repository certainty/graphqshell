{-# LANGUAGE TemplateHaskell #-}

module Shell.Configuration where
import Relude
import Text.URI (URI,mkURI)
import Lens.Micro.Platform (makeLenses, (^.))
import Control.Exception.Safe (MonadThrow, throw)
import Data.Yaml (decodeThrow, withObject, withArray)
import Data.Yaml.Aeson (FromJSON(..), (.:),(.:?),(.!=), Parser, Value)
import Data.Vector hiding (sequence)
import Validation
import Data.List ((\\), nub)
import qualified Data.List.NonEmpty as NonEmpty

newtype ConfigurationError = ConfigurationError [ConfigurationErrorDetail] deriving (Eq, Show)

data ConfigurationErrorDetail =
    MultipleDefaultEndpoints [Text] 
  | MultipleDefaultThemes [Text]
  | DuplicateNames [Text]
  | MissingDefaultEndpoint
  | MissingDefaultTheme
  deriving (Eq, Show)

instance Exception ConfigurationError

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

parseConfiguration :: (MonadThrow m) => ByteString -> m ApplicationConfig
parseConfiguration inp = do
  cfg <- decodeThrow inp
  case validateConfiguration cfg of
    (Failure f) -> throw (ConfigurationError (NonEmpty.toList f))
    (Success validatedCfg) -> pure validatedCfg

validateConfiguration ::  ApplicationConfig -> Validation (NonEmpty ConfigurationErrorDetail) ApplicationConfig
validateConfiguration = validateAll [
  validateSingleDefaultEndpoint,
  validateSingleDefaultTheme,
  validateUniqueEndpointNames,
  validateUniqueThemeNames
  ]
  where
    validateSingleDefaultEndpoint cfg = do
      case Relude.map _endpointName (Relude.filter _endpointIsDefault (cfg ^. appConfigEndpoints)) of
        [ ]       -> failure MissingDefaultEndpoint
        [_]       -> Success cfg
        endpoints -> failure (MultipleDefaultEndpoints endpoints)
        
    validateSingleDefaultTheme cfg = do
      case Relude.map _themeName (Relude.filter _themeIsDefault (cfg ^. appConfigThemes)) of
        [ ]    -> failure MissingDefaultTheme
        [_]    -> Success cfg
        themes -> failure (MultipleDefaultThemes themes)

    validateUniqueEndpointNames cfg = do
      let duplicates = findDuplicates $ Relude.map _endpointName (cfg ^. appConfigEndpoints)
      if Relude.null duplicates then
        Success cfg
      else failure (DuplicateNames duplicates)

    validateUniqueThemeNames cfg = do
      let duplicates = findDuplicates $ Relude.map _themeName (cfg ^. appConfigThemes)
      if Relude.null duplicates then
        Success cfg
      else failure (DuplicateNames duplicates)


    findDuplicates :: (Eq a) => [a] -> [a]
    findDuplicates ls = ls \\ (nub ls) 


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
    
