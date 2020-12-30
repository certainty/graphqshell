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
import Shell.Components.Introspector.Event
import Shell.Components.Types
import Shell.Continuation
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

data State = State
  { _stFieldsView :: FieldViewState,
    _stSelectedType :: ObjectType,
    _stSchema :: Schema
  }
  deriving (Show)

instance Inspect State where
  inspect (State _ selected _) = "State { selected = " <> show (name selected) <> " }"

data FieldViewState = FieldViewState
  { _sfvFields :: L.List () FieldType,
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

initialState :: Schema -> ObjectType -> State
initialState schema selectedType = State fieldViewState selectedType schema
  where
    (ObjectType _ _ fields _) = selectedType
    fieldViewState = FieldViewState (L.list () fields 1) selectedField outputType
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
  State ->
  BrickEvent ComponentName Event ->
  EventM ComponentName (Continuation Event State)
update state (VtyEvent (V.EvKey V.KEnter [])) = case selectedType of
  (Just tpe) -> concurrently state (pure (SelectedTypeChanged tpe))
  Nothing -> keepGoing state
  where
    selectedType = state ^. stFieldsView . sfvSelectedOutputType
update state (VtyEvent ev) = do
  newState <- L.handleListEvent ev (state ^. stFieldsView . sfvFields)
  case L.listSelectedElement newState of
    (Just (_, field)) ->
      keepGoing
        ( state
            & stFieldsView
            .~ ( FieldViewState
                   newState
                   (Just field)
                   (fieldOutputType (state ^. stSchema) field)
               )
        )
    Nothing ->
      keepGoing (state & stFieldsView .~ (FieldViewState newState Nothing Nothing))
update state _ev = keepGoing state

{-
 __     ___
 \ \   / (_) _____      __
  \ \ / /| |/ _ \ \ /\ / /
   \ V / | |  __/\ V  V /
    \_/  |_|\___| \_/\_/

-}

view :: State -> Widget ()
view state = padBottom Max $ vLimitPercent 30 (typeInfoView state) <=> fieldsView state

typeInfoView :: State -> Widget ()
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

fieldsView :: State -> Widget ()
fieldsView state =
  hLimitPercent 30 (hBorderWithLabel (txt " Fields ") <=> fieldsMainView state)
    <+> vBorder
    <+> (hBorderWithLabel (txt "Field Info") <=> fieldsDetailView state)

fieldsMainView :: State -> Widget ()
fieldsMainView state =
  padBottom (Pad 2) $
    L.renderList (renderField state) True (state ^. stFieldsView . sfvFields)

renderField :: State -> Bool -> FieldType -> Widget ()
renderField _state True field = hBox [txt "• ", markup $ toSDL field]
renderField _state _ field = hBox [txt "  ", markup $ toSDL field]

fieldsDetailView :: State -> Widget ()
fieldsDetailView state =
  fieldInfoView state
    <=> hBorderWithLabel (txt " Arguments ")
    <=> fieldArgumentsView state

-- Show information about the selected field
fieldInfoView :: State -> Widget ()
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

fieldArgumentsView :: State -> Widget ()
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
