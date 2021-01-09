module Shell.KeyMap
  ( cmd,
    sub,
    compile,
    KeyMapConfiguration,
    KeyMap,
  )
where

import Control.Exception.Safe (MonadThrow, throw)
import qualified Data.HashMap.Strict as Map
import Relude

newtype KeyMap a = KeyMap {underlying :: Map Char (KeyMapEntry a)}

data KeyMapEntry a
  = Command Text a
  | Group Text (KeyMap a)

data KeyMapConfiguration a = CommandConfig Char Text a | GroupConfig [KeyMapConfiguration a] | SubGroupConfig Char Text (KeyMapConfiguration a)

instance Semigroup (KeyMapConfiguration a) where
  lhs@CommandConfig {} <> rhs@CommandConfig {} = GroupConfig [lhs, rhs]
  (GroupConfig cfgs) <> rhs@CommandConfig {} = GroupConfig (rhs : cfgs)
  lhs@CommandConfig {} <> GroupConfig cfgs = GroupConfig (lhs : cfgs)
  (GroupConfig lhs) <> (GroupConfig rhs) = GroupConfig (lhs ++ rhs)
  lhs@CommandConfig {} <> rhs@SubGroupConfig {} = GroupConfig [lhs, rhs]
  lhs@SubGroupConfig {} <> rhs@CommandConfig {} = GroupConfig [lhs, rhs]
  (GroupConfig lhs) <> rhs@SubGroupConfig {} = GroupConfig (rhs : lhs)
  lhs@SubGroupConfig {} <> GroupConfig rhs = GroupConfig (lhs : rhs)
  lhs@SubGroupConfig {} <> rhs@SubGroupConfig {} = GroupConfig [lhs, rhs]

cmd :: Char -> Text -> a -> KeyMapConfiguration a
cmd = CommandConfig

sub :: Char -> Text -> KeyMapConfiguration a -> KeyMapConfiguration a
sub = SubGroupConfig

compile :: (MonadThrow m) => KeyMapConfiguration a -> m (KeyMap a)
compile = undefined
