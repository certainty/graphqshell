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
import Shell.Components.Types hiding (Event)
import qualified Shell.Components.Types as New
import Shell.Continuation
import qualified Shell.KeyMap as KeyMap

data Event a = CommandSelected a deriving (Eq, Show)

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

updateNew ::
  New.EventChan ->
  State a ->
  BrickEvent ComponentName New.Event ->
  EventM ComponentName (Next (State a))
updateNew chan state@(State rootKeyMap activeKeyMap) (VtyEvent (V.EvKey (V.KChar c) [])) =
  case KeyMap.matchKey activeKeyMap c of
    (Just (KeyMap.Command _ cmd)) -> emitEvent chan state (New.KeyCommand cmd)
    (Just (KeyMap.Group _ keyMap)) -> continue (State rootKeyMap keyMap)
    Nothing -> continue state
updateNew _ s _ = continue s

update ::
  State a ->
  BrickEvent ComponentName (Event a) ->
  EventM ComponentName (Continuation (Event a) (State a))
update state@(State rootKeyMap activeKeyMap) (VtyEvent (V.EvKey (V.KChar c) [])) = undefined
--case KeyMap.matchKey activeKeyMap c of
--(Just (KeyMap.Command _ cmd)) -> concurrently state (pure (CommandSelected cmd))
--(Just (KeyMap.Group _ keyMap)) -> keepGoing (State rootKeyMap keyMap)
--Nothing -> keepGoing state
update s _ = keepGoing s

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
