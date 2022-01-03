module Application.TuiApp.Components.CommandBar.Behavior where

import Application.TuiApp.Components.CommandBar.State
import Application.TuiApp.Shared (Action, Event)
import Brick
import Control.Exception.Safe (MonadThrow)
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
  ( Attr,
  )
import Infrastructure.TuiEngine as Tui hiding (Event)
import qualified Infrastructure.TuiEngine.Events as TuiEvents
import Infrastructure.TuiEngine.Keymap (KeyMap)
import qualified Infrastructure.TuiEngine.Keymap as KeyMap
import Infrastructure.TuiEngine.Keys (Key (Key))
import Lens.Micro.Platform ((^.))
import Relude hiding (State, state)

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

update :: (MonadThrow m) => State -> TuiEvents.Event Event -> m (Continuation State Action Event)
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
