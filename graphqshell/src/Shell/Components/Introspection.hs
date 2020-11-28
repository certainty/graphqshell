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
import Brick.Markup (markup, (@?))
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Brick.Widgets.List as L
import Data.Text.Markup (Markup (..), (@@))
import GraphQL.Introspection.Schema
import GraphQL.Introspection.Schema.Types (HasName (name))
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes (Attr, bold, defAttr, withStyle)
import Lens.Micro ((.~), (^.))
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

-- Custom formatting
attributes :: [(AttrName, Attr)]
attributes = [(L.listSelectedAttr, withStyle defAttr bold)]

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
view state =
  padBottom Max $
    mainView state <+> vBorder <+> detailView state

detailView :: State -> Widget ()
detailView _state = padLeft (Pad 1) $ str "Type"

mainView :: State -> Widget ()
mainView state =
  hLimit 70 $
    padBottom (Pad 2) $
      htitle (name $ state ^. stSelectedType)
        <=> padLeft (Pad 3) (L.renderList (renderField schema) True (state ^. (stFieldView . sfvFields)))
  where
    schema = state ^. stSchema

-- The field data in the mainView
renderField :: Schema -> Bool -> FieldType -> Widget ()
renderField _schema _selected field = markup $ toSDL field

htitle :: Text -> Widget n
htitle t = hBox [padLeft (Pad 1) $ txt t]

-- Helpers will later be extracted

class ToSDL a where
  toSDL :: a -> Markup AttrName

instance ToSDL TypeReference where
  toSDL (ListOf tpe) = "[" <> toSDL tpe <> "]"
  toSDL (NonNullOf tpe) = toSDL tpe <> "!"
  toSDL (NamedType tpeName) = tpeName @? "typeName"
  toSDL UnnamedType = ""

instance ToSDL FieldType where
  toSDL (FieldType fieldName _descr _depr _args outputRef) = (fieldName @? "fieldName") <> "(...): " <> toSDL outputRef
