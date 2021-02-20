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
    (^.),
  )
import Relude hiding
  ( State,
    state,
  )
import qualified Shell.Components.Introspector.ObjectType as IntroObject
import Shell.Components.Shared
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
keyMapConfig = Just (cmd 'q' "Query Type" CmdIntrospectorGotoQuery)

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
  BrickEvent ComponentName Event ->
  State ->
  EventM ComponentName (Next State)
update _ (AppEvent (KeyCommand CmdIntrospectorGotoQuery)) state = do
  _ <- putStrLn "HELLL YEAH"
  continue (initialState (state ^. stSchema) (Object (query (state ^. stSchema))))
update _ (AppEvent (SelectedTypeChanged selectedType)) state = continue (pushSelectedType state selectedType)
update _ (VtyEvent (V.EvKey (V.KChar '[') [])) state = continue (popSelectedType state)
-- TODO: simplify this
update chan (VtyEvent ev) state@(State _ _ (ObjectTypeState tpeState)) = do
  nextCont <- IntroObject.update chan (VtyEvent ev) tpeState
  pure $
    (\newState -> set stSelectedTypeState (ObjectTypeState newState) state)
      <$> nextCont
-- catch all
update _ _ state = continue state

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
