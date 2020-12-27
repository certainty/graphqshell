{-# LANGUAGE TemplateHaskell #-}

module Shell.Components.Introspector
  ( view
  , Event(..)
  , State
  , update
  , attributes
  , initialState
  )
where

import           Brick
import           GraphQL.Introspection.Schema
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Attributes                                                ( Attr
                                                                                        )
import           Lens.Micro.Platform                                                    ( makeLenses
                                                                                        , (^.)
                                                                                        , set
                                                                                        )
import           Relude                                                          hiding ( State
                                                                                        , state
                                                                                        )
import           Shell.Components.Types
import qualified Shell.SDL                     as SDL
import qualified Shell.Components.Introspector.ObjectType
                                               as IntroObject

import           Shell.Components.Utilities
{-
  _____                 _
 | ____|_   _____ _ __ | |_
 |  _| \ \ / / _ \ '_ \| __|
 | |___ \ V /  __/ | | | |_
 |_____| \_/ \___|_| |_|\__|

-}

data Event = ObjectTypeEvent IntroObject.Event
  deriving (Eq, Ord, Show)

{-
  ____  _        _
 / ___|| |_ __ _| |_ ___
 \___ \| __/ _` | __/ _ \
  ___) | || (_| | ||  __/
 |____/ \__\__,_|\__\___|

-}

data State = State
  { _stSchema            :: Schema
  , _stSelectedTypeStack :: [GraphQLType]
  , _stSelectedTypeState :: SelectedTypeState
  }

data SelectedTypeState = ObjectTypeState IntroObject.State
  | UnsupportedTypeState

makeLenses ''State

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
  State schema [(Object tpe)] (ObjectTypeState (IntroObject.initialState schema tpe))
initialState schema tpe = State schema [tpe] UnsupportedTypeState

{-
  _   _           _       _
 | | | |_ __   __| | __ _| |_ ___
 | | | | '_ \ / _` |/ _` | __/ _ \
 | |_| | |_) | (_| | (_| | ||  __/
  \___/| .__/ \__,_|\__,_|\__\___|
       |_|
-}

update :: State -> BrickEvent ComponentName Event -> EventM ComponentName (Next State)
-- Doesn't really belong here,, how do we emit an event in the subcomponent?
update state@(State schema _ (ObjectTypeState tpeState)) (VtyEvent (V.EvKey V.KEnter []))
  = case selectedType of
    (Just tpe) -> continue (initialState schema tpe)
    Nothing    -> continue state
 where
  selectedType = tpeState ^. IntroObject.stFieldsView . IntroObject.sfvSelectedOutputType

update state@(State _ _ (ObjectTypeState tpeState)) (VtyEvent ev) = do
  nextVal <- IntroObject.update tpeState (VtyEvent ev)
  pure
    $   (\newState -> set stSelectedTypeState (ObjectTypeState newState) state)
    <$> nextVal
-- catch all
update state _ev = continue state

{-
 __     ___
 \ \   / (_) _____      __
  \ \ / /| |/ _ \ \ /\ / /
   \ V / | |  __/\ V  V /
    \_/  |_|\___| \_/\_/

-}

view :: State -> Widget ()
view (State _ _ (ObjectTypeState tpeState)) = IntroObject.view tpeState
view _ = unsupportedView

-- | Will go away
unsupportedView :: Widget ()
unsupportedView = padBottom Max $ txt "This type is not yet supported"
