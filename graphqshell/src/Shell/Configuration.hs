
module Shell.Configuration
  (
    module Shell.Configuration.Types,
    module Shell.Configuration.Validation,
    ConfigurationError(..),
    parseConfiguration
   ) where
import Relude
import Control.Exception.Safe (MonadThrow, throw)
import Shell.Configuration.Validation
import Shell.Configuration.Types
import Validation (Validation(Success), Validation(Failure))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Yaml (decodeThrow)

data  ConfigurationError = InvalidConfig [ValidationError] deriving (Eq, Show)
instance Exception ConfigurationError

parseConfiguration :: (MonadThrow m) => ByteString -> m ApplicationConfig
parseConfiguration inp = do
  cfg <- decodeThrow inp
  case validateConfiguration cfg of
    (Failure f) -> throw (InvalidConfig (NonEmpty.toList f))
    (Success validatedCfg) -> pure validatedCfg
