{-# LANGUAGE TemplateHaskell #-}

-- | Provides the command bar compnent
-- which implements which-key like guided / contextual commands
module Shell.Components.CommandBar where

import Brick
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
  ( Attr,
  )
import Lens.Micro.Platform
  ( makeLenses,
  )
import Relude hiding
  ( State,
    state,
  )
import Shell.Components.Shared
import qualified Shell.KeyMap as KeyMap

data State a = State
  { _stRootKeyMap :: KeyMap.KeyMap CommandBarCommand,
    _stActiveKeyMap :: KeyMap.KeyMap CommandBarCommand
  }

makeLenses ''State

{-
     _   _   _        _ _           _
    / \ | |_| |_ _ __(_) |__  _   _| |_ ___  ___
   / _ \| __| __| '__| | '_ \| | | | __/ _ \/ __|
  / ___ \ |_| |_| |  | | |_) | |_| | ||  __/\__ \
 /_/   \_\__|\__|_|  |_|_.__/ \__,_|\__\___||___/

-}

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

{-
  ___       _ _
 |_ _|_ __ (_) |_
  | || '_ \| | __|
  | || | | | | |_
 |___|_| |_|_|\__|

-}

initialState :: KeyMap.KeyMap CommandBarCommand -> State a
initialState keyMap = State keyMap keyMap

resetState :: State a -> State a
resetState (State rootKeyMap _) = State rootKeyMap rootKeyMap

{-
  _   _           _       _
 | | | |_ __   __| | __ _| |_ ___
 | | | | '_ \ / _` |/ _` | __/ _ \
 | |_| | |_) | (_| | (_| | ||  __/
  \___/| .__/ \__,_|\__,_|\__\___|
       |_|

-}

update ::
  EventChan ->
  BrickEvent ComponentName Event ->
  State a ->
  EventM ComponentName (Next (State a))
update chan (VtyEvent (V.EvKey (V.KChar c) [])) state@(State rootKeyMap activeKeyMap) =
  case KeyMap.matchKey activeKeyMap c of
    (Just (KeyMap.Command _ cmd)) -> emitEvent chan state (KeyCommand cmd)
    (Just (KeyMap.Group _ keyMap)) -> continue (State rootKeyMap keyMap)
    Nothing -> continue state
update _ _ s = continue s

{-
 __     ___
 \ \   / (_) _____      __
  \ \ / /| |/ _ \ \ /\ / /
   \ V / | |  __/\ V  V /
    \_/  |_|\___| \_/\_/

-}

view :: State a -> Widget b
view (State _ activeKeyMap) = hBox bindingEntries
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
