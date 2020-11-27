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
import GraphQL.Introspection.Schema.Types (ObjectType (ObjectType))
import qualified Graphics.Vty as V
import Relude hiding (State)

data Event = Event deriving (Eq, Ord, Show)

data State = State Schema GraphQLType (L.List () FieldType)
  deriving (Show)

mkState :: Schema -> State
mkState schema = State schema (query schema) (L.list () fields 1)
  where
    (Object (ObjectType _ _ fields _)) = query schema

update :: State -> BrickEvent n Event -> EventM n (Next State)
update s _ = continue s

view :: Widget ()
view = padBottom Max $ fieldView <+> vBorder <+> typeView

typeView :: Widget ()
typeView = vLimitPercent 70 $ padRight Max (str "typeinfo")

fieldView :: Widget ()
fieldView = vLimitPercent 30 (str "fields")
