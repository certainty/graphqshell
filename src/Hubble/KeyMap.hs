{-
Types and functions to handle key events conveniently

Examples:

@
  import Hubble.KeyMap
  data ProgramKeys = ProgamKeys {
     pkQuit :: Binding,
     pkHelp :: Binding
  }

  mkProgramKeys :: (MonadThrow m) => m ProgramKeys
  mkProgramKeys =
    ProgramKeys
      <$> mkBinding Enabled ["C-q"] (withHelp "q" "Quit the program")
      <*> mkBinding Enabled ["h"] (withHelp "h", "Show help")

  -- then in your update function you can do the following
  myUpdate :: MyModel -> Message MyMessage -> (MyModel, [Mycommand])
  myUpdate model@(MyModel programKeys _ _) (KeyMsg k)
    | (pkQuite programKeys) `matches` k => (model, [Exit])
    | (pkHelp programKeys) `matches` k  => (showHelp model, [])
    | otherwise => (model, [])

  -- more updates on events
@

-}

module Hubble.KeyMap where

import Control.Exception.Safe (MonadThrow, throwString)
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import Relude
import Relude.Unsafe as Unsafe

data BindingState = Enabled | Disabled deriving (Eq, Show)

data Binding = Binding
  { _bindingHelp :: Maybe Help,
    _bindingKeys :: [Key],
    _bindingState :: BindingState
  }
  deriving (Eq, Show)

data Help = Help
  { -- | textual representation of the key that help is for
    _helpKey :: KeyName,
    _helpDescription :: KeyDescription
  }
  deriving (Eq, Show)

newtype Key = Key (Vty.Key, [Vty.Modifier]) deriving (Eq, Show)

type KeyChord = Text

type KeyName = Text

type KeyDescription = Text

mkBinding :: (MonadThrow m) => BindingState -> [KeyChord] -> Maybe Help -> m Binding
mkBinding bindingState chords help = do
  parsedKeys <- traverse parseKeyChord chords
  pure $ Binding help (Key <$> parsedKeys) bindingState

withHelp :: KeyName -> KeyDescription -> Maybe Help
withHelp name descr = Just (Help name descr)

withoutHelp :: Maybe Help
withoutHelp = Nothing

matches :: Binding -> Key -> Bool
matches (Binding _ keys Enabled) k = k `elem` keys
matches _ _ = False

parseKeyChord :: (MonadThrow m) => KeyChord -> m (Vty.Key, [Vty.Modifier])
parseKeyChord chord = do
  (modifiers, keyName) <- splitKeyChord chord
  vtyKey <- parseKeyName keyName
  vtyModifiers <- traverse parseModifier modifiers
  pure (vtyKey, vtyModifiers)

parseModifier :: (MonadThrow m) => Text -> m Vty.Modifier
parseModifier modifier = case modifier of
  "S" -> pure Vty.MShift
  "C" -> pure Vty.MCtrl
  "A" -> pure Vty.MAlt
  "M" -> pure Vty.MMeta
  _ -> throwString $ "Unknown modifier: " <> show modifier

parseKeyName :: (MonadThrow m) => Text -> m Vty.Key
parseKeyName keyName
  | T.length keyName == 1 = pure $ Vty.KChar (Unsafe.head (T.unpack keyName))
  | otherwise = case keyName of
    "<esc>" -> pure Vty.KEsc
    "<return>" -> pure Vty.KEnter
    "<left>" -> pure Vty.KLeft
    "<right>" -> pure Vty.KRight
    "<down>" -> pure Vty.KDown
    "<up>" -> pure Vty.KUp
    "<home>" -> pure Vty.KHome
    "<pageup>" -> pure Vty.KPageUp
    "<pagedown>" -> pure Vty.KPageDown
    "<fn1>" -> pure $ Vty.KFun 1
    "<fn2>" -> pure $ Vty.KFun 2
    "<fn3>" -> pure $ Vty.KFun 3
    "<fn4>" -> pure $ Vty.KFun 4
    "<fn5>" -> pure $ Vty.KFun 5
    "<fn6>" -> pure $ Vty.KFun 6
    "<fn7>" -> pure $ Vty.KFun 7
    "<fn8>" -> pure $ Vty.KFun 8
    "<fn9>" -> pure $ Vty.KFun 9
    "<fn10>" -> pure $ Vty.KFun 10
    "<pause>" -> pure Vty.KPause
    "<begin>" -> pure Vty.KBegin
    "<end>" -> pure Vty.KEnd
    "<del>" -> pure Vty.KDel
    "<backspace>" -> pure Vty.KBS
    "<tab>" -> pure $ Vty.KChar '\t'
    _ -> throwString $ "Unknown key name: " <> show keyName

splitKeyChord :: (MonadThrow m) => KeyChord -> m ([Text], Text)
splitKeyChord keyChord
  | T.null keyChord = throwString "Empty key chord"
  | T.length keyChord == 1 = pure ([], keyChord)
  | otherwise =
    let parts = T.splitOn "-" keyChord
     in case splitAt (length parts - 1) parts of
          (modifiers, [k]) -> pure (modifiers, k)
          _ -> throwString $ "Invalid key chord: " <> show keyChord
