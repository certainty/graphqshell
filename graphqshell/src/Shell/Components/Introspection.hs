{-# LANGUAGE TemplateHaskell #-}

module Shell.Components.Introspection
  ( view
  , Event(..)
  , State
  , mkState
  , update
  , attributes
  )
where

import           Brick
import           Brick.Markup                   ( markup
                                                , (@?)
                                                )
import           Brick.Widgets.Border
import qualified Brick.Widgets.List            as L
import           Data.Text.Markup               ( fromText )
import qualified Data.Text.Markup              as Markup
import           Data.Vector                    ( (!?) )
import           Data.Vector.Generic            ( foldl )
import           GraphQL.Introspection.Schema
import           GraphQL.Introspection.Schema.Types
                                                ( HasName(name)
                                                , description
                                                )
import           Graphics.Vty                   ( standout )
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Attributes        ( Attr
                                                , bold
                                                , defAttr
                                                , withStyle
                                                )
import           Lens.Micro.Platform            ( makeLenses
                                                , (.~)
                                                , (^.)
                                                )
import           Relude                  hiding ( State
                                                , state
                                                )
import           Shell.Components.Types

data Event = Event deriving (Eq, Ord, Show)

data State = State
  { _stSchema :: Schema,
    _stSelectedType :: GraphQLType,
    _stFieldView :: Maybe FieldViewState
  }

data FieldViewState = FieldViewState
  { _sfvFields :: L.List () FieldType,
    _sfvSelectedField :: Maybe FieldType,
    _sfvSelectedOutputType :: Maybe GraphQLType
  }

-- schema helpers may be extracted later
fieldOutputType :: Schema -> FieldType -> Maybe GraphQLType
fieldOutputType schema (FieldType _ _ _ _ ref) = lookupType ref schema

mkFieldViewState :: Schema -> ObjectType -> FieldViewState
mkFieldViewState schema selectedType = FieldViewState (L.list () fields 1)
                                                      selectedField
                                                      selectedOutputType
 where
    -- can also be something else
  (ObjectType _ _ fields _) = selectedType
  selectedField             = fields !? 0
  selectedOutputType        = selectedField >>= fieldOutputType schema

makeLenses ''State
makeLenses ''FieldViewState

-- Custom formatting
attributes :: [(AttrName, Attr)]
attributes =
  [ (L.listSelectedAttr, withStyle defAttr bold)
  , ("standout"        , withStyle defAttr standout)
  , (attrSDLIdentifier , defAttr)
  , (attrSDLKeyword    , defAttr)
  , (attrSDLParens     , defAttr)
  ]

mkState :: Schema -> GraphQLType -> State
mkState schema selectedType@(Object tpe) =
  State schema selectedType (Just (mkFieldViewState schema tpe))
mkState schema selectedType = State schema selectedType Nothing

-- TODO: extract code to update subcomponents
update
  :: State
  -> BrickEvent ComponentName Event
  -> EventM ComponentName (Next State)
update state@(State schema _ (Just fieldViewState)) (VtyEvent (V.EvKey V.KEnter []))
  = case selectedType of
    (Just tpe) -> continue (mkState schema tpe)
    Nothing    -> continue state
  where selectedType = fieldViewState ^. sfvSelectedOutputType
update state@(State _ _ (Just fieldViewState)) (VtyEvent ev) = do
  newState <- L.handleListEvent ev (fieldViewState ^. sfvFields)
  case L.listSelectedElement newState of
    (Just (_, field)) -> continue
      (state & stFieldView .~ Just
        (FieldViewState newState
                        (Just field)
                        (fieldOutputType (state ^. stSchema) field)
        )
      )
    Nothing ->
      continue
        (state & stFieldView .~ Just (FieldViewState newState Nothing Nothing))

-- catch all
update state _ev = continue state

-- View
view :: State -> Widget ()
view (State schema selectedType (Just fieldViewState)) =
  objectTypeView selectedType schema fieldViewState
view _ = unsupportedTypeView

-- | Will go away
unsupportedTypeView :: Widget ()
unsupportedTypeView = padBottom Max $ txt "This type is not yet supported"

-- | ObjectType view
objectTypeView :: GraphQLType -> Schema -> FieldViewState -> Widget ()
objectTypeView selectedType schema state =
  padBottom Max
    $   vLimitPercent 30 (objectTypeInfoView selectedType)
    <=> objectTypeFieldsView schema state

objectTypeInfoView :: GraphQLType -> Widget ()
objectTypeInfoView selectedType =
  (   info "Type"          typeName
    <+> info "Kind"          typeKind
    <+> info "Referenced by" referencedBy
    <+> info "References"    references
    )
    <=> infoWidget "Description" (txtWrap (txtOpt typeDescr))
 where
  typeName     = Just $ name $ selectedType
  typeKind     = Just (graphQLKind selectedType)
  referencedBy = Nothing
  references   = Nothing
  typeDescr    = description selectedType

info :: Text -> Maybe Text -> Widget n
info label value = infoWidget label (txt (txtOpt value))

infoWidget :: Text -> Widget n -> Widget n
infoWidget label value = padAll 1 $ txt (label <> ": ") <+> value

graphQLKind :: GraphQLType -> Text
graphQLKind (Object    _) = "Object"
graphQLKind (Scalar    _) = "Scalar"
graphQLKind (Enum      _) = "Enum"
graphQLKind (Interface _) = "Interface"
graphQLKind (Union     _) = "Union"
graphQLKind (Input     _) = "Input Object"

txtOpt :: Maybe Text -> Text
txtOpt = fromMaybe "N/A"

-- | ObjectType fields view
-- This is a basic main - detail architecture
objectTypeFieldsView :: Schema -> FieldViewState -> Widget ()
objectTypeFieldsView schema state =
  hLimitPercent
      30
      (   hBorderWithLabel (txt " Fields ")
      <=> objectTypeFieldsMainView schema state
      )
    <+> vBorder
    <+> (hBorderWithLabel (txt "Field Info") <=> objectTypeFieldDetailView state
        )

objectTypeFieldsMainView :: Schema -> FieldViewState -> Widget ()
objectTypeFieldsMainView schema state = padBottom (Pad 2)
  $ L.renderList (renderField schema) True (state ^. sfvFields)

objectTypeFieldDetailView :: FieldViewState -> Widget ()
objectTypeFieldDetailView state =
  fieldInfoView state
    <=> hBorderWithLabel (txt " Arguments ")
    <=> fieldArgumentsView state

fieldInfoView :: FieldViewState -> Widget ()
fieldInfoView state =
  info "Name" fieldName
    <+> info "Type"     fieldTypeName
    <+> info "Nullable" nullableInfo
    <=> infoWidget "Description" (txtWrap (txtOpt fieldDescription))
 where
  fieldName        = name <$> selectedField
  fieldDescription = selectedField >>= description
  fieldTypeName    = Markup.toText . toSDL <$> fieldType
  nullableInfo     = show . isNullable <$> fieldType
  selectedField    = state ^. sfvSelectedField
  fieldType        = fieldTypeReference <$> selectedField

fieldArgumentsView :: FieldViewState -> Widget ()
fieldArgumentsView _state = txt "Arguments view comes later"

-- The field data in the mainView
renderField :: Schema -> Bool -> FieldType -> Widget ()
renderField _schema True field = hBox [txt "• ", markup $ toSDL field]
renderField _schema _    field = hBox [txt "  ", markup $ toSDL field]

-- Helpers will later be extracted

class ToSDL a where
  toSDL :: a -> Markup.Markup AttrName

instance ToSDL TypeReference where
  toSDL (ListOf    tpe                ) = "[" <> toSDL tpe <> "]"
  toSDL (NonNullOf tpe                ) = toSDL tpe <> "!"
  toSDL (Named     (NamedType tpeName)) = tpeName @? attrSDLIdentifier

instance ToSDL FieldType where
  toSDL (FieldType fieldName _descr _depr _args outputRef) =
    (fieldName @? attrSDLIdentifier) <> "(...): " <> toSDL outputRef

instance ToSDL GraphQLType where
  toSDL (Object (ObjectType tpeName _descr fields _interfaces)) =
    ("type" @? attrSDLKeyword)
      <> " "
      <> fromText tpeName
      <> " "
      <> ("{" @? attrSDLParens)
      <> "\n"
      <> sdlFields
      <> ("}" @? attrSDLParens)
   where
    sdlFields = foldl sdlField "" fields
    sdlField accu field = accu <> "  " <> toSDL field <> "\n"
  toSDL _ = "unsupported"

attrSDLParens :: AttrName
attrSDLParens = "sdl" <> "paren"

attrSDLKeyword :: AttrName
attrSDLKeyword = "sdl" <> "keyword"

attrSDLIdentifier :: AttrName
attrSDLIdentifier = "sdl" <> "identifier"
