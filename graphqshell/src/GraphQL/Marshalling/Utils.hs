module GraphQL.Marshalling.Utils
  ( aesonOptions,
  )
where

import Data.Aeson as J
import Data.Char (toLower)
import Relude

-- | Aeson options that strip field prefixes
--
-- @
-- instance FromJSON MyType where
--   parseJSON = genericParseJSON (aesonOptions "prefix")
-- @
aesonOptions :: String -> Options
aesonOptions prefix = J.defaultOptions {J.fieldLabelModifier = rewriteFieldName}
  where
    rewriteFieldName fieldName = case drop (length prefix) fieldName of
      (front : rear) -> toLower front : rear
      _ -> fieldName
