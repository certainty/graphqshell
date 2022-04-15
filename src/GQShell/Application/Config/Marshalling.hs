{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}

-- | Lowlevel marshalling for configuration data
--   The final configuration is unmarshalled, validated and post processed separately, as we
--   this needs whole file analysis.
module GQShell.Application.Config.Marshalling where

import qualified Data.Text.Encoding as Encoding
import qualified Data.Vector as Vector
import Data.Yaml
  ( withArray,
    withObject,
  )
import Data.Yaml.Aeson
  ( FromJSON (..),
    Parser,
    ToJSON (toJSON),
    Value,
    (.!=),
    (.:),
    (.:?),
  )
import Relude
import Text.URI
  ( URI,
    mkURI,
  )

data ConfigFile = ConfigFile (Maybe ApplicationEntry) [EndpointEntry] deriving (Show, Generic)

type TickRate = Int

newtype ApplicationEntry = ApplicationEntry (Maybe TickRate) deriving (Show, Generic)

type Name = Text

data EndpointEntry = EndpointEntry Name Bool URI (Maybe URI) (Maybe EndpointHttpEntry) deriving (Eq, Show, Generic)

type Headers = [(ByteString, ByteString)]

newtype EndpointHttpEntry = EndpointHttpEntry (Maybe Headers) deriving (Eq, Show, Generic)

instance FromJSON ApplicationEntry where
  parseJSON = withObject "ApplicationEntry" $ \o -> do
    ApplicationEntry <$> o .:? "tickrate"

-- Marshalling
instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \o -> do
    ConfigFile <$> o .:? "application" <*> o .: "endpoints"

instance FromJSON EndpointEntry where
  parseJSON = withObject "EndpointEntry" $ \o -> do
    EndpointEntry
      <$> (o .: "name")
      <*> (o .:? "default" .!= False)
      <*> ((o .: "url") >>= parseURI)
      <*> ((o .:? "link") >>= parseMaybeURI)
      <*> (o .:? "http")
    where
      parseMaybeURI u = sequence $ parseURI <$> u
      parseURI uri = case mkURI uri :: Maybe URI of
        (Just u) -> pure u
        _ -> fail "Could not parse URI"

instance FromJSON EndpointHttpEntry where
  parseJSON = withObject "EndpointHttpEntry" $ \o -> do
    headers <- o .:? "custom-headers"
    parsedHeaders <- sequence (parseCustomHeaders <$> headers)
    pure $ EndpointHttpEntry parsedHeaders
    where
      parseCustomHeaders :: Value -> Parser [(ByteString, ByteString)]
      parseCustomHeaders inp =
        Vector.toList <$> withArray "headers" (traverse parsePairs) inp
      parsePairs :: Value -> Parser (ByteString, ByteString)
      parsePairs = withObject "header" $ \o -> do
        key <- Encoding.encodeUtf8 <$> (o .: "name")
        val <- Encoding.encodeUtf8 <$> (o .: "value")
        pure (key, val)
