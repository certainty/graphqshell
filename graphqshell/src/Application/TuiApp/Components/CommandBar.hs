{-# LANGUAGE TemplateHaskell #-}

-- | Provides the command bar compnent
-- which implements which-key like guided / contextual commands
module Application.TuiApp.Components.CommandBar where

import Application.TuiApp.Shared
import Brick
import Control.Exception.Safe (MonadThrow)
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
  ( Attr,
  )
import Infrastructure.TuiEngine (Component (Component, _componentInitial, _componentName, _componentUpdate, _componentView), Continuation)
import qualified Infrastructure.TuiEngine as Tui
import Infrastructure.TuiEngine.Components (Continuation (Continue, Notify))
import Infrastructure.TuiEngine.Keymap hiding (cmd)
import qualified Infrastructure.TuiEngine.Keymap as KeyMap
import Infrastructure.TuiEngine.Keys
import Lens.Micro.Platform
  ( makeLenses,
    (^.),
  )
import Relude hiding
  ( State,
    state,
  )

data State = State
  { _stRootKeyMap :: KeyMap Event,
    _stActiveKeyMap :: KeyMap Event
  }

makeLenses ''State

type ComponentType m = Component State Action Event ComponentName m

component :: (MonadThrow m) => KeyMap Event -> Component State Action Event ComponentName m
component keyMap =
  Component
    { _componentName = CommandBar,
      _componentInitial = initial keyMap,
      _componentUpdate = update,
      _componentView = Just view
    }

attrDescription :: AttrName
attrDescription = "commandBar" <> "description"

attrCommand :: AttrName
attrCommand = "commandBar" <> "command"

attrSeparator :: AttrName
attrSeparator = "commandBar" <> "separator"

attrGroup :: AttrName
attrGroup = "commandBar" <> "group"

attributes :: [(AttrName, Attr)]
attributes =
  [ (attrDescription, V.defAttr),
    (attrCommand, V.defAttr),
    (attrSeparator, V.defAttr),
    (attrGroup, V.defAttr)
  ]

initial :: (MonadThrow m) => KeyMap Event -> m (Continuation State Action Event)
initial keyMap = pure (Continue (State keyMap keyMap))

resetState :: State -> State
resetState (State rootKeyMap _) = State rootKeyMap rootKeyMap

withKeyMap :: State -> KeyMap Event -> State
withKeyMap (State rootKeyMap _) = State rootKeyMap

update :: (MonadThrow m) => State -> Tui.Event Event -> m (Continuation State Action Event)
update state (Tui.EventInputKey (Key c)) =
  case KeyMap.matchKey (state ^. stActiveKeyMap) c of
    (Just (KeyMap.Command _ cmd)) -> pure $ Notify state cmd -- just emit the corresponding event
    (Just (KeyMap.Group _ keyMap)) -> pure $ Continue (withKeyMap state keyMap) -- set the new keymap and continue
    Nothing -> pure $ Continue state
update s _ = pure $ Continue s

view :: State -> [Widget b]
view (State _ activeKeyMap) = [hBox bindingEntries]
  where
    bindingEntries = map bindingEntry (KeyMap.bindings activeKeyMap)
    bindingEntry (c, descr) =
      padRight (Pad 2) $
        padLeft (Pad 2) $
          hBox
            [ withAttr attrCommand $ txt (translateKey c),
              withAttr attrSeparator $ padLeft (Pad 1) $ txt separator,
              withAttr attrDescription $ padLeft (Pad 1) $ txt descr
            ]
    separator = "→"
    translateKey ' ' = "<SPACE>"
    translateKey c = Text.singleton c
