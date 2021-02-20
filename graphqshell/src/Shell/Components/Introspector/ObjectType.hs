{-# LANGUAGE TemplateHaskell #-}

-- | Introspector for object types
--
-- This is probably one of the most complex introspectors.
-- It provides information about object types and their fields.
--
-- As every component exposes the three main functions `initialState`, `update`, `view`
module Shell.Components.Introspector.ObjectType where

import Brick
import Brick.Markup
  ( markup,
  )
import Brick.Widgets.Border
import qualified Brick.Widgets.List as L
import qualified Data.Text.Markup as Markup
import Data.Vector
  ( (!?),
  )
import GraphQL.Introspection.Schema
import GraphQL.Introspection.Schema.Types
  ( HasName
      ( name
      ),
    description,
  )
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
  ( Attr,
    bold,
    defAttr,
    withStyle,
  )
import Lens.Micro.Platform
  ( makeLenses,
    (.~),
    (^.),
  )
import Relude hiding
  ( State,
    state,
  )
import Shell.Components.Shared
import Shell.SDL hiding
  ( attributes,
  )
import Utils

{-
  ____  _        _
 / ___|| |_ __ _| |_ ___
 \___ \| __/ _` | __/ _ \
  ___) | || (_| | ||  __/
 |____/ \__\__,_|\__\___|

-}

data State a = State
  { _stFieldsView :: FieldViewState a,
    _stSelectedType :: ObjectType,
    _stSchema :: Schema
  }
  deriving (Show)

instance Inspect (State a) where
  inspect (State _ selected _) = "State { selected = " <> show (name selected) <> " }"

data FieldViewState a = FieldViewState
  { _sfvFields :: L.List a FieldType,
    _sfvSelectedField :: Maybe FieldType,
    _sfvSelectedOutputType :: Maybe GraphQLType
  }
  deriving (Show)

makeLenses ''FieldViewState
makeLenses ''State

{-

     _   _   _        _ _           _
    / \ | |_| |_ _ __(_) |__  _   _| |_ ___  ___
   / _ \| __| __| '__| | '_ \| | | | __/ _ \/ __|
  / ___ \ |_| |_| |  | | |_) | |_| | ||  __/\__ \
 /_/   \_\__|\__|_|  |_|_.__/ \__,_|\__\___||___/

-}

attributes :: [(AttrName, Attr)]
attributes = [(L.listSelectedAttr, withStyle defAttr bold)]

{-
  ___       _ _
 |_ _|_ __ (_) |_
  | || '_ \| | __|
  | || | | | | |_
 |___|_| |_|_|\__|

-}

initialState :: a -> Schema -> ObjectType -> State a
initialState resource schema selectedType = State fieldViewState selectedType schema
  where
    (ObjectType _ _ fields _) = selectedType
    fieldViewState = FieldViewState (L.list resource fields 1) selectedField outputType
    selectedField = fields !? 0
    outputType = selectedField >>= fieldOutputType schema

{-
  _   _           _       _
 | | | |_ __   __| | __ _| |_ ___
 | | | | '_ \ / _` |/ _` | __/ _ \
 | |_| | |_) | (_| | (_| | ||  __/
  \___/| .__/ \__,_|\__,_|\__\___|
       |_|

-}

update ::
  (Ord a) =>
  EventChan ->
  State a ->
  BrickEvent a Event ->
  EventM a (Next (State a))
update chan state (VtyEvent (V.EvKey V.KEnter [])) = case selectedType of
  (Just tpe) -> emitEvent chan state (SelectedTypeChanged tpe)
  Nothing -> continue state
  where
    selectedType = state ^. stFieldsView . sfvSelectedOutputType
update chan state (VtyEvent ev) = do
  newState <- L.handleListEvent ev (state ^. stFieldsView . sfvFields)
  case L.listSelectedElement newState of
    (Just (_, field)) ->
      continue
        ( state
            & stFieldsView
            .~ ( FieldViewState
                   newState
                   (Just field)
                   (fieldOutputType (state ^. stSchema) field)
               )
        )
    Nothing ->
      continue (state & stFieldsView .~ (FieldViewState newState Nothing Nothing))
update _ state _ev = continue state

{-
 __     ___
 \ \   / (_) _____      __
  \ \ / /| |/ _ \ \ /\ / /
   \ V / | |  __/\ V  V /
    \_/  |_|\___| \_/\_/

-}

view :: (Ord a, Show a) => State a -> Widget a
view state = padBottom Max $ vLimitPercent 30 (typeInfoView state) <=> hBorder <=> fieldsView state

typeInfoView :: State a -> Widget a
typeInfoView state =
  ( info "Type" typeName
      <+> info "Kind" (Just "Object")
      <+> info "Referenced by" referencedBy
      <+> info "References" references
  )
    <=> infoWidget "Description" (txtWrap (txtOpt typeDescr))
  where
    typeName = Just $ name $ selectedType
    referencedBy = Nothing
    references = Nothing
    typeDescr = description selectedType
    selectedType = state ^. stSelectedType

fieldsView :: (Ord a, Show a) => State a -> Widget a
fieldsView state =
  hLimitPercent 30 (hBorderWithLabel (txt " Fields ") <=> fieldsMainView state)
    <+> vBorder
    <+> (hBorderWithLabel (txt "Field Info") <=> fieldsDetailView state)

fieldsMainView :: (Ord a, Show a) => State a -> Widget a
fieldsMainView state =
  padBottom (Pad 2) $
    L.renderList (renderField state) True (state ^. stFieldsView . sfvFields)

renderField :: State a -> Bool -> FieldType -> Widget a
renderField _state True field = hBox [txt "• ", markup $ toSDL field]
renderField _state _ field = hBox [txt "  ", markup $ toSDL field]

fieldsDetailView :: State a -> Widget a
fieldsDetailView state =
  fieldInfoView state
    <=> hBorderWithLabel (txt " Arguments ")
    <=> fieldArgumentsView state

-- Show information about the selected field
fieldInfoView :: State a -> Widget a
fieldInfoView state =
  info "Name" fieldName
    <+> info "Type" fieldTypeName
    <+> info "Nullable" nullableInfo
    <=> infoWidget "Description" (txtWrap (txtOpt fieldDescription))
  where
    fieldName = name <$> selectedField
    fieldDescription = selectedField >>= description
    fieldTypeName = Markup.toText . toSDL <$> fieldType
    nullableInfo = show . isNullable <$> fieldType
    fieldType = fieldTypeReference <$> selectedField
    selectedField = state ^. stFieldsView . sfvSelectedField

fieldArgumentsView :: State a -> Widget a
fieldArgumentsView _state = txt "Arguments view comes later"

-- View helpers
info :: Text -> Maybe Text -> Widget n
info label value = infoWidget label (txt (txtOpt value))

infoWidget :: Text -> Widget n -> Widget n
infoWidget label value = padAll 1 $ txt (label <> ": ") <+> value

-- TODO: extract into common view helpers
txtOpt :: Maybe Text -> Text
txtOpt = fromMaybe "N/A"

-- Type info helpers

graphQLKind :: GraphQLType -> Text
graphQLKind (Object _) = "Object"
graphQLKind (Scalar _) = "Scalar"
graphQLKind (Enum _) = "Enum"
graphQLKind (Interface _) = "Interface"
graphQLKind (Union _) = "Union"
graphQLKind (Input _) = "Input Object"

fieldOutputType :: Schema -> FieldType -> Maybe GraphQLType
fieldOutputType schema (FieldType _ _ _ _ ref) = lookupType ref schema
