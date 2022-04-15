{-# LANGUAGE TemplateHaskell #-}

module GQShell.Application.Config.Types where

import Data.Default
import Lens.Micro.Platform (makeLenses)
import Relude
import Text.URI (URI, emptyURI)

data ApplicationConfig = ApplicationConfig
  { _appConfigTickRate :: Maybe Int,
    _appConfigDefaultEndpoint :: EndpointConfig,
    _appConfigEndpoints :: [EndpointConfig]
  }
  deriving (Generic, Eq)

data EndpointConfig = EndpointConfig
  { _endpointName :: Text,
    _endpointIsDefault :: Bool,
    _endpointURL :: URI,
    _endpointLink :: Maybe URI,
    _endpointHttpConfig :: Maybe EndpointHttpConfig
  }
  deriving (Generic, Show, Eq)

newtype EndpointHttpConfig = EndpointHttpConfig
  { _endpointHttpHeaders :: Maybe [(ByteString, ByteString)]
  }
  deriving (Generic, Show, Eq)

makeLenses ''ApplicationConfig
makeLenses ''EndpointConfig
makeLenses ''EndpointHttpConfig

class IsDefaultConfig a where
  isDefault :: a -> Bool

instance IsDefaultConfig EndpointConfig where
  isDefault = _endpointIsDefault

instance Default EndpointHttpConfig where
  def = EndpointHttpConfig Nothing

instance Default EndpointConfig where
  def =
    EndpointConfig
      { _endpointName = "empty",
        _endpointIsDefault = False,
        _endpointURL = emptyURI,
        _endpointLink = Nothing,
        _endpointHttpConfig = Nothing
      }

instance Default ApplicationConfig where
  def =
    ApplicationConfig
      { _appConfigTickRate = Just 100,
        _appConfigDefaultEndpoint = def,
        _appConfigEndpoints = [def]
      }
