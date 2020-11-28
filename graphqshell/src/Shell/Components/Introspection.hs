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
import Data.Text.Markup (Markup (..), fromText, (@@))
import Data.Vector (Vector, (!?))
import Data.Vector.Generic (foldl)
import GraphQL.Introspection.Schema
import GraphQL.Introspection.Schema.Types (HasName (name), description)
import Graphics.Vty (standout)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes (Attr, bold, defAttr, withStyle)
import Lens.Micro.Platform (makeLenses, (.~), (^.))
import Relude hiding (State, state)
import Shell.Components.Types

data Event = Event deriving (Eq, Ord, Show)

data State = State
  { _stSchema :: Schema,
    _stSelectedType :: GraphQLType,
    _stFieldView :: FieldViewState
  }

data FieldViewState = FieldViewState
  { _sfvFields :: L.List () FieldType,
    _sfvSelectedField :: Maybe FieldType,
    _sfvSelectedOutputType :: Maybe GraphQLType
  }

makeLenses ''State
makeLenses ''FieldViewState

-- Custom formatting
attributes :: [(AttrName, Attr)]
attributes = [(L.listSelectedAttr, withStyle defAttr bold), ("standout", withStyle defAttr standout)]

mkState :: Schema -> State
mkState schema = State schema (query schema) (FieldViewState (L.list () fields 1) selectedField selectedOutputType)
  where
    (Object (ObjectType _ _ fields _)) = query schema
    selectedField = fields !? 0
    selectedOutputType = selectedField >>= fieldOutputType schema

update :: State -> BrickEvent ComponentName Event -> EventM ComponentName (Next State)
update state (VtyEvent ev) = do
  newState <- L.handleListEvent ev (state ^. (stFieldView . sfvFields))
  case L.listSelectedElement newState of
    (Just (_, field)) -> continue (state & stFieldView .~ FieldViewState newState (Just field) (fieldOutputType (state ^. stSchema) field))
    Nothing -> continue (state & stFieldView .~ FieldViewState newState Nothing Nothing)
-- catch all
update state _ev = continue state

-- schema helpers may be extracted later
fieldOutputType :: Schema -> FieldType -> Maybe GraphQLType
fieldOutputType schema (FieldType _ _ _ _ ref) = lookupType ref schema

-- View
view :: State -> Widget ()
view state =
  padBottom Max $
    mainView state <+> vBorder <+> detailView state

mainView :: State -> Widget ()
mainView state =
  hLimitPercent 40 $
    padBottom (Pad 2) $
      htitle (name $ state ^. stSelectedType)
        <=> padLeft (Pad 3) (L.renderList (renderField schema) True (state ^. (stFieldView . sfvFields)))
  where
    schema = state ^. stSchema

detailView :: State -> Widget ()
detailView state =
  padLeft (Pad 2) (htitle (selectedTypeName <> "." <> fieldName))
    <=> hBorder
    <=> padLeft (Pad 2) (htitle "Description")
    <=> padLeft (Pad 2) fieldDescription
    <=> hBorder
    <=> padLeft (Pad 2) fieldType
  where
    fieldDescription = vBox [txt (fromMaybe "" (selectedField >>= description))]
    fieldType = markup (maybe "" toSDL selectedOutputType)
    selectedField = state ^. (stFieldView . sfvSelectedField)
    selectedOutputType = state ^. (stFieldView . sfvSelectedOutputType)
    fieldName = maybe "" name selectedField
    selectedTypeName = name $ state ^. stSelectedType

-- The field data in the mainView
renderField :: Schema -> Bool -> FieldType -> Widget ()
renderField _schema True field = hBox [txt "• ", markup $ toSDL field]
renderField _schema _ field = hBox [txt "  ", markup $ toSDL field]

htitle :: Text -> Widget n
htitle t = hBox [padBottom (Pad 1) $ txt t]

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

instance ToSDL GraphQLType where
  toSDL (Object (ObjectType tpeName _descr fields _interfaces)) = ("type" @? "keyword") <> " " <> fromText tpeName <> " " <> ("{" @? "paren") <> "\n" <> sdlFields <> ("}" @? "paren")
    where
      sdlFields = foldl sdlField "" fields
      sdlField accu field = accu <> "  " <> toSDL field <> "\n"
  toSDL _ = "unsupported"
