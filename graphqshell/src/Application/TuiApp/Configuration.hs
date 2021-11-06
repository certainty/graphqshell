module Application.TuiApp.Configuration
  ( module Application.TuiApp.Configuration.Types,
    Application.TuiApp.Configuration.Validation.ValidationError (..),
    ConfigurationError (..),
    parseConfiguration,
  )
where

import Application.TuiApp.Configuration.Types
import Application.TuiApp.Configuration.Validation
import Control.Exception.Safe
  ( MonadThrow,
    throw,
  )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Yaml
  ( decodeThrow,
  )
import Relude
import Validation
  ( Validation
      ( Failure,
        Success
      ),
  )

data ConfigurationError = InvalidConfig [ValidationError]
  deriving (Eq, Show)

instance Exception ConfigurationError

parseConfiguration :: (MonadThrow m) => FilePath -> ByteString -> m ApplicationConfig
parseConfiguration configFilePath fileContent = do
  cfg <- decodeThrow fileContent
  case fromMarshalled configFilePath cfg of
    (Failure f) -> throw (InvalidConfig (NonEmpty.toList f))
    (Success validatedCfg) -> pure validatedCfg
