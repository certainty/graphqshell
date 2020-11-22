-- |
module Utils
  ( throwEither,
  )
where

import Control.Exception.Safe (MonadThrow, throw)
import Relude

throwEither :: (MonadThrow m, Exception a) => Either a b -> m b
throwEither (Left e) = throw e
throwEither (Right v) = pure v
