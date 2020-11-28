{-# LANGUAGE TemplateHaskell #-}

module Shell.Components.Introspection
  ( view,
    Event (..),
    State,
    mkState,
    update,
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import qualified Brick.Widgets.List as L
import GraphQL.Introspection.Schema (FieldType, GraphQLType (..), Schema, query)
import GraphQL.Introspection.Schema.Types (ObjectType (ObjectType), name)
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Relude hiding (State, state)

data Event = Event deriving (Eq, Ord, Show)

data State = State
  { _stSchema :: Schema,
    _stSelectedType :: GraphQLType,
    _stFieldView :: FieldViewState
  }

data FieldViewState = FieldViewState
  { _sfvFields :: (L.List () FieldType)
  }

makeLenses ''State
makeLenses ''FieldViewState

mkState :: Schema -> State
mkState schema = State schema (query schema) (FieldViewState (L.list () fields 1))
  where
    (Object (ObjectType _ _ fields _)) = query schema

update :: State -> BrickEvent n Event -> EventM n (Next State)
update s _ = continue s

view :: State -> Widget ()
view state = padBottom Max $ fieldView state <+> vBorder <+> typeView state

typeView :: State -> Widget ()
typeView _state = str "Type"

fieldView :: State -> Widget ()
fieldView state = L.renderList renderField True (state ^. (stFieldView . sfvFields))

renderField :: Bool -> FieldType -> Widget ()
renderField _selected field = str (toString (name field))
