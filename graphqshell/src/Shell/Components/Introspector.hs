{-# LANGUAGE TemplateHaskell #-}

module Shell.Components.Introspector
  ( view,
    Event (..),
    State,
    update,
    attributes,
    initialState,
    keyMapConfig,
  )
where

import Brick
import GraphQL.Introspection.Schema
import GraphQL.Introspection.Schema.Types (name)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
  ( Attr,
  )
import Lens.Micro.Platform
  ( makeLenses,
    set,
  )
import Relude hiding
  ( State,
    state,
  )
import qualified Shell.Components.Introspector.ObjectType as IntroObject
import Shell.Components.Types
import Shell.Continuation
import Shell.KeyMap
import qualified Shell.SDL as SDL
import Utils

{-
  ____  _        _
 / ___|| |_ __ _| |_ ___
 \___ \| __/ _` | __/ _ \
  ___) | || (_| | ||  __/
 |____/ \__\__,_|\__\___|

-}

data State = State
  { _stSchema :: Schema,
    _stSelectedTypeStack :: [GraphQLType],
    _stSelectedTypeState :: SelectedTypeState
  }
  deriving (Show)

instance Inspect State where
  inspect (State _ typeStack s) = " IntrospectorState { stack: " <> show (map name typeStack) <> " selected = " <> inspect s <> "}"

data SelectedTypeState
  = ObjectTypeState (IntroObject.State ComponentName)
  | UnsupportedTypeState
  deriving (Show)

instance Inspect SelectedTypeState where
  inspect (ObjectTypeState tpe) = "ObjectState { tpe = " <> (inspect tpe) <> " }"
  inspect UnsupportedTypeState = "UnsupportedTypeState"

makeLenses ''State

{-
  _  __
 | |/ /___ _  _ _ __  __ _ _ __
 | ' </ -_) || | '  \/ _` | '_ \
 |_|\_\___|\_, |_|_|_\__,_| .__/
           |__/           |_|

-}
keyMapConfig :: Maybe (KeyMapConfiguration CommandBarCommand)
keyMapConfig = Just (cmd 'f' "Test" CmdNoop)

{-
     _   _   _        _ _           _
    / \ | |_| |_ _ __(_) |__  _   _| |_ ___  ___
   / _ \| __| __| '__| | '_ \| | | | __/ _ \/ __|
  / ___ \ |_| |_| |  | | |_) | |_| | ||  __/\__ \
 /_/   \_\__|\__|_|  |_|_.__/ \__,_|\__\___||___/

-}

attributes :: [(AttrName, Attr)]
attributes = IntroObject.attributes ++ SDL.attributes

{-
  ___       _ _
 |_ _|_ __ (_) |_
  | || '_ \| | __|
  | || | | | | |_
 |___|_| |_|_|\__|

-}

initialState :: Schema -> GraphQLType -> State
initialState schema (Object tpe) =
  State schema [(Object tpe)] (ObjectTypeState (IntroObject.initialState IntrospectorComponent schema tpe))
initialState schema tpe = State schema [tpe] UnsupportedTypeState

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
  State ->
  Event ->
  EventM ComponentName (Next State)
update chan state evt = continue state

{-
update ::
  State ->
  BrickEvent ComponentName Event ->
  EventM ComponentName (Continuation Event State)
update state (AppEvent (SelectedTypeChanged selectedType)) = keepGoing (pushSelectedType state selectedType)
update state (VtyEvent (V.EvKey (V.KChar '[') [])) = keepGoing (popSelectedType state)
update state@(State _ _ (ObjectTypeState tpeState)) (VtyEvent ev) = do
  nextCont <- IntroObject.update tpeState (VtyEvent ev)
  pure $
    (\newState -> set stSelectedTypeState (ObjectTypeState newState) state)
      <$> nextCont
-- catch all
update state _ev = keepGoing state
-}

selectedTypeState :: Schema -> GraphQLType -> SelectedTypeState
selectedTypeState schema (Object tpe) = ObjectTypeState (IntroObject.initialState IntrospectorComponent schema tpe)
selectedTypeState _ _ = UnsupportedTypeState

-- | Manage the type stack and state
pushSelectedType :: State -> GraphQLType -> State
pushSelectedType (State schema typeStack _) tpe = State schema (tpe : typeStack) (selectedTypeState schema tpe)

popSelectedType :: State -> State
popSelectedType (State schema [tpe] _) = State schema [tpe] (selectedTypeState schema tpe)
popSelectedType (State schema (_ : prev : rest) _) = State schema (prev : rest) (selectedTypeState schema prev)
popSelectedType s = s

{-
 __     ___
 \ \   / (_) _____      __
  \ \ / /| |/ _ \ \ /\ / /
   \ V / | |  __/\ V  V /
    \_/  |_|\___| \_/\_/

-}

view :: State -> Widget ComponentName
view (State _ _ (ObjectTypeState tpeState)) = IntroObject.view tpeState
view _ = unsupportedView

-- | Will go away
unsupportedView :: Widget ComponentName
unsupportedView = padBottom Max $ txt "This type is not yet supported"
