module Shell.SDL where
import           Relude
import           Brick.AttrMap                                                          ( AttrName
                                                                                        )
import           Graphics.Vty.Attributes                                                ( Attr
                                                                                        )
import qualified Data.Text.Markup              as Markup
import           GraphQL.Introspection.Schema
import           Brick.Markup                                                           ( (@?)
                                                                                        )
import           Data.Vector.Generic                                                    ( foldl
                                                                                        )

import           Data.Text.Markup                                                       ( fromText
                                                                                        )
import           Graphics.Vty.Attributes                                                ( defAttr
                                                                                        )


 {-

     _   _   _        _ _           _
    / \ | |_| |_ _ __(_) |__  _   _| |_ ___  ___
   / _ \| __| __| '__| | '_ \| | | | __/ _ \/ __|
  / ___ \ |_| |_| |  | | |_) | |_| | ||  __/\__ \
 /_/   \_\__|\__|_|  |_|_.__/ \__,_|\__\___||___/


-}

attributes :: [(AttrName, Attr)]
attributes =
  [(attrSDLIdentifier, defAttr), (attrSDLKeyword, defAttr), (attrSDLParens, defAttr)]

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
