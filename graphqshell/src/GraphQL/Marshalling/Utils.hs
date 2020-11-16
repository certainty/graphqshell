module GraphQL.Marshalling.Utils
  (
    aesonOptions 
  ) where
import Relude
import Data.Aeson as J
import Data.Char (toLower)

-- | Aeson options that strip field prefixes

-- Example:
--
-- ```haskel
-- instance FromJSON MyType where
--   parseJSON = genericParseJSON (easonOptions "prefix")
-- ```
--   
--
aesonOptions :: String -> Options
aesonOptions prefix = J.defaultOptions { J.fieldLabelModifier = rewriteFieldName }
  where
    rewriteFieldName fieldName = case (drop (length prefix) fieldName) of
      (front:rear) -> (toLower front) : rear
      _            -> fieldName 
