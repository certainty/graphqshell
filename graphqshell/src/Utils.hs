-- |
module Utils
  ( throwEither,
    Inspect(..)
  )
where

import Control.Exception.Safe (MonadThrow, throw)
import Relude
import qualified Data.Text as T

throwEither :: (MonadThrow m, Exception a) => Either a b -> m b
throwEither (Left e) = throw e
throwEither (Right v) = pure v


-- | A type class similar to show but meant for inspection of types
class Inspect a where
  inspect :: a -> Text


instance Inspect a => Inspect [a] where
  inspect elements = "[" <> T.intercalate ", " (map inspect elements) <> "]"
