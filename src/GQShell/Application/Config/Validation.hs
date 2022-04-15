{-# LANGUAGE RankNTypes #-}

-- | Validions for the application configuration
module GQShell.Application.Config.Validation
  ( fromMarshalled,
    ValidationError (..),
  )
where

import Data.List
  ( nub,
    (\\),
  )
import qualified Data.List as List
import qualified Data.Text as Text
import GQShell.Application.Config.Marshalling
import GQShell.Application.Config.Types hiding
  ( endpointName,
  )
import Relude
import Validation

data ValidationError
  = TickRateOutOfBound Int
  | EmptyName
  | MissingEndpoints -- no endpoints configured
  | MultipleDefaultEndpoints [Text]
  | DuplicateEndpointNames [Text]
  | MissingDefaultEndpoint
  deriving (Eq, Show)

type ValidationResult a = (Validation (NonEmpty ValidationError) a)

fromMarshalled ::
  ConfigFile -> -- the unmarshalled configuration
  ValidationResult ApplicationConfig
fromMarshalled (ConfigFile appConfig endpoints) =
  ApplicationConfig
    <$> validateAppConfig appConfig
    <*> validateDefaultEndpoint endpoints
    <*> validateEndpoints endpoints

validateAppConfig :: Maybe ApplicationEntry -> ValidationResult (Maybe Int)
validateAppConfig Nothing = pure Nothing
validateAppConfig (Just (ApplicationEntry Nothing)) = pure Nothing
validateAppConfig (Just (ApplicationEntry (Just tickRate)))
  | tickRate < 100 = failure (TickRateOutOfBound tickRate)
  | otherwise = pure (Just tickRate)

validateEndpoints :: [EndpointEntry] -> ValidationResult [EndpointConfig]
validateEndpoints [] = failure MissingEndpoints
validateEndpoints endpoints =
  case validateAll collectionValidations endpoints of
    (Success _) -> traverse validateEndpoint endpoints
    (Failure e) -> Failure e
  where
    collectionValidations =
      [validateUniqueEndpointNames, validateExactlyOneDefaultEndpoint]

validateUniqueEndpointNames :: [EndpointEntry] -> ValidationResult ()
validateUniqueEndpointNames endpoints = case findDuplicates of
  [] -> pure () -- (traverse validateEndpoint endpoints)
  duplicates -> failure (DuplicateEndpointNames duplicates)
  where
    findDuplicates = names \\ nub names
    names = map endpointName endpoints

validateExactlyOneDefaultEndpoint ::
  [EndpointEntry] -> Validation (NonEmpty ValidationError) ()
validateExactlyOneDefaultEndpoint endpoints =
  case Relude.filter isDefaultEndpoint endpoints of
    [] -> failure MissingDefaultEndpoint
    [_] -> pure ()
    tooMany -> failure (MultipleDefaultEndpoints (map endpointName tooMany))

validateEndpoint :: EndpointEntry -> ValidationResult EndpointConfig
validateEndpoint (EndpointEntry name defaultFlag uri link httpConfig) =
  EndpointConfig
    <$> validateName
    <*> pure defaultFlag
    <*> validateUri
    <*> validateLink
    <*> validateHttpConfig httpConfig
  where
    validateName = name <$ failureIf (Text.null name) EmptyName
    validateUri = pure uri
    validateLink = pure link

validateHttpConfig ::
  Maybe EndpointHttpEntry -> ValidationResult (Maybe EndpointHttpConfig)
validateHttpConfig (Just (EndpointHttpEntry headers)) =
  pure $ EndpointHttpConfig . traverse validateHeader <$> headers
  where
    validateHeader = pure
validateHttpConfig Nothing = pure Nothing

validateDefaultEndpoint :: [EndpointEntry] -> ValidationResult EndpointConfig
validateDefaultEndpoint [] = failure MissingEndpoints
validateDefaultEndpoint entries = case List.find isDefaultEndpoint entries of
  (Just ep) -> validateEndpoint ep
  Nothing -> failure MissingDefaultEndpoint

endpointName :: EndpointEntry -> Text
endpointName (EndpointEntry name _ _ _ _) = name

isDefaultEndpoint :: EndpointEntry -> Bool
isDefaultEndpoint (EndpointEntry _ defaultFlag _ _ _) = defaultFlag
