module Application.Tui.Keymap
  ( cmd,
    sub,
    fromConfiguration,
    matchKey,
    bindings,
    insertBinding,
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

newtype KeyMapConfigError = DuplicateKeyDefined Char deriving (Eq, Show)

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

fromConfiguration :: (MonadThrow m) => KeyMapConfiguration a -> m (KeyMap a)
fromConfiguration (CommandConfig c descr action) = pure $ KeyMap $ HashMap.fromList [(c, Command descr action)]
fromConfiguration (SubGroupConfig c descr groupCfg) = do
  compiledGroup <- fromConfiguration groupCfg
  pure $ KeyMap (HashMap.singleton c (Group descr compiledGroup))
fromConfiguration (GroupConfig cfg) = KeyMap <$> foldM fromConfiguration' HashMap.empty cfg

fromConfiguration' :: (MonadThrow m) => HashMap Char (KeyMapEntry a) -> KeyMapConfiguration a -> m (HashMap Char (KeyMapEntry a))
fromConfiguration' hashMap (CommandConfig c descr action)
  | HashMap.member c hashMap = throw (DuplicateKeyDefined c)
  | otherwise = pure (HashMap.insert c (Command descr action) hashMap)
fromConfiguration' hashMap (SubGroupConfig c descr groupCfg)
  | HashMap.member c hashMap = throw (DuplicateKeyDefined c)
  | otherwise = do
    compiled <- fromConfiguration groupCfg
    pure $ HashMap.insert c (Group descr compiled) hashMap
fromConfiguration' hashMap (GroupConfig groupCfg) = foldM fromConfiguration' hashMap groupCfg

matchKey :: KeyMap a -> Char -> Maybe (KeyMapEntry a)
matchKey (KeyMap keyMap) char = HashMap.lookup char keyMap

insertBinding :: KeyMap a -> Char -> KeyMapEntry a -> KeyMap a
insertBinding (KeyMap keyMap) c entry = KeyMap (HashMap.insert c entry keyMap)

-- | Returns sorted list of keys in the current keymap
bindings :: KeyMap a -> [(Char, Text)]
bindings (KeyMap keyMap) = sortOn fst $ map extractBindings (HashMap.toList keyMap)
  where
    extractBindings (c, Command descr _) = (c, descr)
    extractBindings (c, Group descr _) = (c, descr)
