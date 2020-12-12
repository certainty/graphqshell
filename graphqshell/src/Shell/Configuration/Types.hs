{-# LANGUAGE TemplateHaskell #-}

module Shell.Configuration.Types where
import           Relude
import           Text.URI                       ( URI
                                                , mkURI
                                                )
import           Data.Yaml                      ( withObject
                                                , withArray
                                                )
import           Data.Yaml.Aeson                ( FromJSON(..)
                                                , (.:)
                                                , (.:?)
                                                , (.!=)
                                                , Parser
                                                , Value
                                                )
import           Lens.Micro.Platform            ( makeLenses )
import qualified Data.Text.Encoding            as ByteString
import qualified Data.Vector                   as Vector

data ApplicationConfig = ApplicationConfig {
  _defaultEndpointConfig :: EndpointConfig,
  _defaultThemeConfig :: ThemeConfig,
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
  _endpointHttpHeaders :: Maybe [(ByteString, ByteString)]
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


class IsDefaultConfig a where
  isDefault :: a -> Bool

instance IsDefaultConfig EndpointConfig where
  isDefault = _endpointIsDefault

instance IsDefaultConfig ThemeConfig where
  isDefault = _themeIsDefault

-- Marshalling
instance FromJSON ApplicationConfig where
  parseJSON = withObject "ApplicationConfig" $ \o -> do
    endpoints       <- o .: "endpoints"
    themes          <- o .: "themes"
    defaultTheme    <- findDefault "themes" themes
    defaultEndpoint <- findDefault "endpoints" endpoints
    pure (ApplicationConfig defaultEndpoint defaultTheme endpoints themes)
   where
    findDefault section items = do
      case Relude.find isDefault items of
        Nothing ->
          fail
            (  "No default in "
            <> section
            <> " configured. Make sure you defined one."
            )
        (Just elt) -> pure elt

instance FromJSON EndpointConfig where
  parseJSON = withObject "EndpointConfig" $ \o -> do
    EndpointConfig
      <$> (o .: "name")
      <*> (o .:? "default" .!= False)
      <*> ((o .: "url") >>= parseURI)
      <*> ((o .:? "link") >>= parseMaybeURI)
      <*> (o .:? "http")
   where
    parseMaybeURI u = sequence $ parseURI <$> u
    parseURI uri = case (mkURI uri) :: Maybe URI of
      (Just u) -> pure u
      _        -> fail "Could not parse URI"

instance FromJSON EndpointHttpConfig where
  parseJSON = withObject "EndpointHttpConfig" $ \o -> do
    headers       <- o .:? "custom-headers"
    parsedHeaders <- sequence $ (parseCustomHeaders <$> headers)
    pure $ EndpointHttpConfig parsedHeaders
   where
    parseCustomHeaders :: Value -> Parser [(ByteString, ByteString)]
    parseCustomHeaders inp =
      Vector.toList <$> withArray "headers" (traverse parsePairs) inp
    parsePairs :: Value -> Parser (ByteString, ByteString)
    parsePairs = withObject "header" $ \o -> do
      key <- ByteString.encodeUtf8 <$> (o .: "name")
      val <- ByteString.encodeUtf8 <$> (o .: "value")
      pure (key, val)

instance FromJSON ThemeConfig where
  parseJSON = withObject "ThemeConfig" $ \o -> do
    ThemeConfig <$> o .: "name" <*> o .:? "default" .!= False <*> o .: "path"
