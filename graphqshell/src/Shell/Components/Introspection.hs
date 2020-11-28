{-# LANGUAGE TemplateHaskell #-}

module Shell.Components.Introspection
  ( view,
    Event (..),
    State,
    mkState,
    update,
    attributes,
  )
where

import Brick
import Brick.AttrMap (AttrName)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import qualified Brick.Widgets.List as L
import GraphQL.Introspection.Schema (FieldType, GraphQLType (..), Schema, query)
import GraphQL.Introspection.Schema.Types (ObjectType (ObjectType), name)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes (Attr)
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Relude hiding (State, state)
import Shell.Components.Types

data Event = Event deriving (Eq, Ord, Show)

data State = State
  { _stSchema :: Schema,
    _stSelectedType :: GraphQLType,
    _stFieldView :: FieldViewState
  }

data FieldViewState = FieldViewState
  { _sfvFields :: L.List () FieldType
  }

makeLenses ''State
makeLenses ''FieldViewState

attributes :: [(AttrName, Attr)]
attributes = []

mkState :: Schema -> State
mkState schema = State schema (query schema) (FieldViewState (L.list () fields 1))
  where
    (Object (ObjectType _ _ fields _)) = query schema

update :: State -> BrickEvent ComponentName Event -> EventM ComponentName (Next State)
update state (VtyEvent ev) = do
  newState <- L.handleListEvent ev (state ^. (stFieldView . sfvFields))
  continue (state & stFieldView .~ FieldViewState newState)
update state _ev = continue state

view :: State -> Widget ()
view state = padBottom Max $ fieldView state <+> vBorder <+> typeView state

typeView :: State -> Widget ()
typeView _state = str "Type"

fieldView :: State -> Widget ()
fieldView state = L.renderList renderField True (state ^. (stFieldView . sfvFields))

renderField :: Bool -> FieldType -> Widget ()
renderField selected field = str $ (toString (name field)) ++ (show selected)

htitle t = hLimit 20 $ withAttr "infoTitle" $ txt t
