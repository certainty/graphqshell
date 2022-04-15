module GQShell.Application.Config
  ( module GQShell.Application.Config.Types,
    GQShell.Application.Config.Validation.ValidationError (..),
    ConfigurationError (..),
    parseConfiguration,
  )
where

import Control.Exception.Safe
  ( MonadThrow,
    throw,
  )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Yaml
  ( decodeThrow,
  )
import GQShell.Application.Config.Types
import GQShell.Application.Config.Validation
import Relude
import Validation
  ( Validation
      ( Failure,
        Success
      ),
  )

newtype ConfigurationError = InvalidConfig [ValidationError]
  deriving (Eq, Show)

instance Exception ConfigurationError

parseConfiguration :: (MonadThrow m) => ByteString -> m ApplicationConfig
parseConfiguration fileContent = do
  cfg <- decodeThrow fileContent
  case fromMarshalled cfg of
    (Failure f) -> throw (InvalidConfig (NonEmpty.toList f))
    (Success validatedCfg) -> pure validatedCfg
