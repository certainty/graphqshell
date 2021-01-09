module Shell.KeyMap
  ( cmd,
    sub,
    compile,
    matchKey,
    bindings,
    KeyMapConfiguration,
    KeyMap,
    KeyMapEntry (..),
    KeyMapConfigError (..),
  )
where

import Control.Exception.Safe (MonadThrow, throw)
import Control.Monad (foldM)
import qualified Data.HashMap.Strict as HashMap
import Relude

data KeyMapConfigError = DuplicateKeyDefined Char deriving (Eq, Show)

instance Exception KeyMapConfigError

newtype KeyMap a = KeyMap {underlying :: HashMap Char (KeyMapEntry a)} deriving (Eq, Show)

data KeyMapEntry a
  = Command Text a
  | Group Text (KeyMap a)
  deriving (Eq, Show)

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
compile (CommandConfig c descr action) = pure $ KeyMap $ HashMap.fromList [(c, Command descr action)]
compile (SubGroupConfig c descr groupCfg) = do
  compiledGroup <- compile groupCfg
  pure $ KeyMap (HashMap.singleton c (Group descr compiledGroup))
compile (GroupConfig cfg) = KeyMap <$> foldM compile' HashMap.empty cfg

compile' :: (MonadThrow m) => HashMap Char (KeyMapEntry a) -> KeyMapConfiguration a -> m (HashMap Char (KeyMapEntry a))
compile' hashMap (CommandConfig c descr action)
  | HashMap.member c hashMap = throw (DuplicateKeyDefined c)
  | otherwise = pure (HashMap.insert c (Command descr action) hashMap)
compile' hashMap (SubGroupConfig c descr groupCfg)
  | HashMap.member c hashMap = throw (DuplicateKeyDefined c)
  | otherwise = do
    compiled <- compile groupCfg
    pure $ HashMap.insert c (Group descr compiled) hashMap
compile' hashMap (GroupConfig groupCfg) = foldM compile' hashMap groupCfg

matchKey :: KeyMap a -> Char -> Maybe (KeyMapEntry a)
matchKey (KeyMap keyMap) char = HashMap.lookup char keyMap

-- | Returns sorted list of keys in the current keymap
bindings :: KeyMap a -> [(Char, Text)]
bindings (KeyMap keyMap) = map extractBindings (HashMap.toList keyMap)
  where
    extractBindings (c, (Command descr _)) = (c, descr)
    extractBindings (c, (Group descr _)) = (c, descr)
