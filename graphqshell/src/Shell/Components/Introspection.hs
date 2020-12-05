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
import qualified Data.Text.Markup as Markup
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

-- schema helpers may be extracted later
fieldOutputType :: Schema -> FieldType -> Maybe GraphQLType
fieldOutputType schema (FieldType _ _ _ _ ref) = lookupType ref schema

mkFieldViewState :: Schema -> GraphQLType -> FieldViewState
mkFieldViewState schema selectedType = FieldViewState (L.list () fields 1) selectedField selectedOutputType
  where
    -- can also be something else
    (Object (ObjectType _ _ fields _)) = selectedType
    selectedField = fields !? 0
    selectedOutputType = selectedField >>= fieldOutputType schema

makeLenses ''State
makeLenses ''FieldViewState

-- Custom formatting
attributes :: [(AttrName, Attr)]
attributes = [(L.listSelectedAttr, withStyle defAttr bold), ("standout", withStyle defAttr standout)]

mkState :: Schema -> GraphQLType -> State
mkState schema selectedType = State schema selectedType (mkFieldViewState schema selectedType)

update :: State -> BrickEvent ComponentName Event -> EventM ComponentName (Next State)
update state (VtyEvent (V.EvKey V.KEnter [])) = case selectedType of
  (Just tpe) -> continue (mkState (state ^. stSchema) tpe)
  Nothing -> continue state
  where
    selectedType = state ^. stFieldView . sfvSelectedOutputType
update state (VtyEvent ev) = do
  newState <- L.handleListEvent ev (state ^. (stFieldView . sfvFields))
  case L.listSelectedElement newState of
    (Just (_, field)) -> continue (state & stFieldView .~ FieldViewState newState (Just field) (fieldOutputType (state ^. stSchema) field))
    Nothing -> continue (state & stFieldView .~ FieldViewState newState Nothing Nothing)
-- catch all
update state _ev = continue state

-- View
view :: State -> Widget ()
view = objectTypeView

-- | ObjectType view
objectTypeView :: State -> Widget ()
objectTypeView state =
  padBottom Max $
    vLimitPercent 30 (objectTypeInfoView state)
      <=> objectTypeFieldsView state

objectTypeInfoView :: State -> Widget ()
objectTypeInfoView state = (info "Type" typeName <+> info "Kind" typeKind <+> info "Referenced by" referencedBy <+> info "References" references) <=> infoWidget "Description" (txtWrap (txtOpt typeDescr))
  where
    typeName = Just $ name $ state ^. stSelectedType
    typeKind = Just (graphQLKind $ state ^. stSelectedType)
    referencedBy = Nothing
    references = Nothing
    typeDescr = description $ state ^. stSelectedType

info :: Text -> Maybe Text -> Widget n
info label value = infoWidget label (txt (txtOpt value))

infoWidget :: Text -> Widget n -> Widget n
infoWidget label value = padAll 1 $ txt (label <> ": ") <+> value

graphQLKind :: GraphQLType -> Text
graphQLKind (Object _) = "Object"
graphQLKind (Scalar _) = "Scalar"
graphQLKind (Enum _) = "Enum"
graphQLKind (Interface _) = "Interface"
graphQLKind (Union _) = "Union"
graphQLKind (Input _) = "Input Object"

txtOpt :: Maybe Text -> Text
txtOpt = fromMaybe "N/A"

-- | ObjectType fields view
-- This is a basic main - detail architecture
objectTypeFieldsView :: State -> Widget ()
objectTypeFieldsView state =
  hLimitPercent 30 (hBorderWithLabel (txt " Fields ") <=> objectTypeFieldsMainView state) <+> vBorder <+> (hBorderWithLabel (txt "Field Info") <=> objectTypeFieldDetailView state)

objectTypeFieldsMainView :: State -> Widget ()
objectTypeFieldsMainView state =
  padBottom (Pad 2) $
    L.renderList (renderField schema) True (state ^. (stFieldView . sfvFields))
  where
    schema = state ^. stSchema

objectTypeFieldDetailView :: State -> Widget ()
objectTypeFieldDetailView state =
  fieldInfoView (state ^. stFieldView)
    <=> hBorderWithLabel (txt " Arguments ")
    <=> fieldArgumentsView (state ^. stFieldView)

fieldInfoView :: FieldViewState -> Widget ()
fieldInfoView state = info "Name" fieldName <+> info "Type" fieldTypeName <+> info "Nullable" nullableInfo <=> infoWidget "Description" (txtWrap (txtOpt fieldDescription))
  where
    fieldName = name <$> selectedField
    fieldDescription = selectedField >>= description
    fieldTypeName = Markup.toText . toSDL <$> fieldType
    nullableInfo = show . isNullable <$> fieldType
    selectedField = state ^. sfvSelectedField
    fieldType = fieldTypeReference <$> selectedField

fieldArgumentsView :: FieldViewState -> Widget ()
fieldArgumentsView state = txt "Arguments view comes later"
  where
    selectedField = state ^. sfvSelectedField

-- The field data in the mainView
renderField :: Schema -> Bool -> FieldType -> Widget ()
renderField _schema True field = hBox [txt "• ", markup $ toSDL field]
renderField _schema _ field = hBox [txt "  ", markup $ toSDL field]

-- Helpers will later be extracted

class ToSDL a where
  toSDL :: a -> Markup AttrName

instance ToSDL TypeReference where
  toSDL (ListOf tpe) = "[" <> toSDL tpe <> "]"
  toSDL (NonNullOf tpe) = toSDL tpe <> "!"
  toSDL (Named (NamedType tpeName)) = tpeName @? "typeName"

instance ToSDL FieldType where
  toSDL (FieldType fieldName _descr _depr _args outputRef) = (fieldName @? "fieldName") <> "(...): " <> toSDL outputRef

instance ToSDL GraphQLType where
  toSDL (Object (ObjectType tpeName _descr fields _interfaces)) = ("type" @? "keyword") <> " " <> fromText tpeName <> " " <> ("{" @? "paren") <> "\n" <> sdlFields <> ("}" @? "paren")
    where
      sdlFields = foldl sdlField "" fields
      sdlField accu field = accu <> "  " <> toSDL field <> "\n"
  toSDL _ = "unsupported"
