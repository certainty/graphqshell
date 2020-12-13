{-# LANGUAGE TemplateHaskell #-}

module Shell.Configuration.Types where
import           Relude
import           Text.URI                       ( URI )
import           Lens.Micro.Platform            ( makeLenses )

data ApplicationConfig = ApplicationConfig {
  _appConfigTickRate :: Maybe Int,
  _appConfigDefaultEndpoint :: EndpointConfig,
  _appConfigDefaultTheme :: ThemeConfig,
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
