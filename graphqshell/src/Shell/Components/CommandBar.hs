{-# LANGUAGE TemplateHaskell #-}

-- which implements which-key like guided / contextual commands

-- | Provides the command bar compnent
module Shell.Components.CommandBar where

import Brick
import qualified Brick.Focus as Focus
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified GraphQL.API as API
import GraphQL.Introspection.Schema
  ( GraphQLType
      ( Object
      ),
    Schema,
    query,
  )
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
  ( Attr,
  )
import Lens.Micro.Platform
  ( makeLenses,
    (^.),
  )
import Relude hiding
  ( State,
    state,
  )
import Shell.Components.Types
import Shell.Continuation
import Shell.KeyMap

type Event = ()

data State a = State
  { _stRootKeyMap :: KeyMap a,
    _stActiveKeyMap :: KeyMap a
  }

makeLenses ''State

{-
     _   _   _        _ _           _
    / \ | |_| |_ _ __(_) |__  _   _| |_ ___  ___
   / _ \| __| __| '__| | '_ \| | | | __/ _ \/ __|
  / ___ \ |_| |_| |  | | |_) | |_| | ||  __/\__ \
 /_/   \_\__|\__|_|  |_|_.__/ \__,_|\__\___||___/

-}

attributes :: [(AttrName, Attr)]
attributes =
  [ ("commandBar" <> "description", V.defAttr),
    ("commandBar" <> "command", V.defAttr),
    ("commandBar" <> "separator", V.defAttr),
    ("commandBar" <> "group", V.defAttr)
  ]

{-
  ___       _ _
 |_ _|_ __ (_) |_
  | || '_ \| | __|
  | || | | | | |_
 |___|_| |_|_|\__|

-}

initialState :: KeyMap a -> State a
initialState keyMap = State keyMap keyMap

{-
  _   _           _       _
 | | | |_ __   __| | __ _| |_ ___
 | | | | '_ \ / _` |/ _` | __/ _ \
 | |_| | |_) | (_| | (_| | ||  __/
  \___/| .__/ \__,_|\__,_|\__\___|
       |_|

-}

update ::
  State a ->
  BrickEvent ComponentName Event ->
  EventM ComponentName (Continuation Event (State a))
update = undefined

{-
 __     ___
 \ \   / (_) _____      __
  \ \ / /| |/ _ \ \ /\ / /
   \ V / | |  __/\ V  V /
    \_/  |_|\___| \_/\_/

-}

view :: State a -> Widget b
view (State _ activeKeyMap) = hBox [padRight Max $ padLeft (Pad 1) $ txt "The Command Bar"]