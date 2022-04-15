{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module GQShell.Application.TUI.Shared where

import Brick (Widget)
import Control.Exception.Safe (MonadThrow)
import GQShell.Application.Config (EndpointConfig)
import GQShell.Core.GraphQL.Introspection.Schema (Schema)
import Hubble.KeyMap (Binding, BindingState (Enabled), mkBinding, withHelp)
import Hubble.Program (Message, Model, UpdateM, mState, update, view)
import Lens.Micro.Platform (Lens', makeLenses)
import qualified Lens.Micro.Platform as Lens
import Relude

data EndpointState = Connecting EndpointConfig | Connected EndpointConfig Schema | Disconnected deriving (Eq, Show)

data AppMessage
  = EndpointChange EndpointState
  | EndpointError Text
  deriving (Eq, Show)

data AppCommand
  = -- | A command that is used to emit a constant message which is not the result of the interpretation of the command.
    --   This is used to send notifications to other components
    EmitMessages [AppMessage]
  | ConnectEndpoint EndpointConfig
  deriving (Eq, Show)

-- Main key bindings that exist independently of the activity
data ProgramKeys = ProgramKeys
  { _pkQuit :: Binding,
    _pkHelp :: Binding,
    _pkToggleLogs :: Binding,
    _pkToggleEndpointMenu :: Binding
  }

mkProgramKeys :: (MonadThrow m) => m ProgramKeys
mkProgramKeys =
  ProgramKeys
    <$> mkBinding Enabled ["C-q", "C-c"] (withHelp "q" "Quit the app")
    <*> mkBinding Enabled ["h", "H"] (withHelp "h" "Help")
    <*> mkBinding Enabled ["T"] (withHelp "T" "toggle logs")
    <*> mkBinding Enabled ["C-e"] (withHelp "C-e" "Toggle Endpointmenu")

-- Model fixed for the applications message and command type
type Model' s = Model s AppCommand AppMessage

type Message' = Message AppMessage

type Command' = AppCommand

-- Focus handling

data Focus = Focus | Unfocus deriving (Eq, Show)

class Focusable a where
  focusL :: Lens' a Focus
  hasFocus :: a -> Bool
  hasFocus s = Lens.view focusL s == Focus
  focusOn :: a -> a
  focusOn = Lens.set focusL Focus
  focusOff :: a -> a
  focusOff = Lens.set focusL Unfocus
  toggleFocus :: a -> a
  toggleFocus = toggleFocusL focusL

toggleFocusL :: Lens' a Focus -> a -> a
toggleFocusL l d
  | hasFocusL l d = focusOffL l d
  | otherwise = focusOnL l d

hasFocusL :: Lens' a Focus -> a -> Bool
hasFocusL l = (== Focus) . Lens.view l

focusOnL :: Lens' a Focus -> a -> a
focusOnL l = Lens.set l Focus

focusOffL :: Lens' a Focus -> a -> a
focusOffL l = Lens.set l Unfocus

instance (Focusable s) => Focusable (Model' s) where
  focusL = mState . focusL

-- Manage things that have a visibility

-- TODO: think about extracting that to Hubble instead

data Visibility = Visible | Hidden deriving (Eq, Show)

data Tile m = Tile
  { _tVisibility :: Visibility,
    _tData :: m
  }
  deriving (Show)

makeLenses ''Tile

instance Functor Tile where
  fmap f (Tile v m) = Tile v (f m)

instance (Focusable m) => Focusable (Tile m) where
  focusL = tData . focusL

updateTile :: Tile (Model s cmd msg) -> Message msg -> UpdateM (Tile (Model s cmd msg), [cmd])
updateTile (Tile Visible m) msg = do
  (updatedState, commands) <- update m msg
  pure (Tile Visible updatedState, commands)
updateTile tile _ = pure (tile, [])

-- Force view on the tile even if it hasn't got focus
updateTile' :: Tile (Model s cmd msg) -> Message msg -> UpdateM (Tile (Model s cmd msg), [cmd])
updateTile' (Tile v m) msg = do
  (updatedState, commands) <- update m msg
  pure (Tile v updatedState, commands)

-- View the tile if it's visible
viewTile :: Tile (Model' m) -> Maybe (Widget ())
viewTile (Tile Visible m) = Just $ view m
viewTile (Tile Hidden _) = Nothing

viewTile' :: Tile (Model' m) -> Widget ()
viewTile' (Tile _ m) = view m

makeTile :: m -> Tile m
makeTile = Tile Visible

isVisible :: Tile m -> Bool
isVisible (Tile Visible _) = True
isVisible _ = False

setVisible :: Tile m -> Tile m
setVisible (Tile _ m) = Tile Visible m

setInvisible :: Tile m -> Tile m
setInvisible (Tile _ m) = Tile Hidden m

toggleVisibility :: Tile m -> Tile m
toggleVisibility (Tile Visible m) = Tile Hidden m
toggleVisibility (Tile Hidden m) = Tile Visible m

class HasKeyMap a where
  getBindings :: a -> [Binding]
