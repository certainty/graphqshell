{-# LANGUAGE RankNTypes #-}

-- | Validions for the application configuration
module Application.TuiApp.Configuration.Validation
  ( fromMarshalled,
    ValidationError (..),
  )
where

import Application.TuiApp.Configuration.Marshalling
import Application.TuiApp.Configuration.Types hiding
  ( endpointName,
    themeName,
  )
import Data.List
  ( nub,
    (\\),
  )
import qualified Data.List as List
import qualified Data.Text as Text
import Relude
import System.FilePath
  ( isRelative,
    normalise,
    takeDirectory,
    (</>),
  )
import Validation

data ValidationError
  = TickRateOutOfBound Int
  | EmptyName
  | MissingEndpoints -- no endpoints configured
  | MissingThemes -- no themes configured
  | MultipleDefaultEndpoints [Text]
  | MultipleDefaultThemes [Text]
  | DuplicateEndpointNames [Text]
  | DuplicateThemeNames [Text]
  | MissingDefaultEndpoint
  | MissingDefaultTheme
  deriving (Eq, Show)

type ValidationResult a = (Validation (NonEmpty ValidationError) a)

fromMarshalled ::
  FilePath -> -- the path from which the config was loaded
  ConfigFile -> -- the unmarshalled configuration
  ValidationResult ApplicationConfig
fromMarshalled configFilePath (ConfigFile appConfig endpoints themes) =
  ApplicationConfig
    <$> (validateAppConfig appConfig)
    <*> (validateDefaultEndpoint endpoints)
    <*> (validateDefaultTheme themes configFilePath)
    <*> (validateEndpoints endpoints)
    <*> (validateThemes themes configFilePath)

validateAppConfig :: Maybe ApplicationEntry -> ValidationResult (Maybe Int)
validateAppConfig Nothing = pure Nothing
validateAppConfig (Just (ApplicationEntry Nothing)) = pure Nothing
validateAppConfig (Just (ApplicationEntry (Just tickRate)))
  | tickRate < 100 = failure (TickRateOutOfBound tickRate)
  | otherwise = pure (Just tickRate)

validateEndpoints :: [EndpointEntry] -> ValidationResult [EndpointConfig]
validateEndpoints [] = failure MissingEndpoints
validateEndpoints endpoints =
  case (validateAll collectionValidations) endpoints of
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
    findDuplicates = names \\ (nub names)
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
    <*> (validateHttpConfig httpConfig)
  where
    validateName = name <$ failureIf (Text.null name) EmptyName
    validateUri = pure uri
    validateLink = pure link

validateHttpConfig ::
  Maybe EndpointHttpEntry -> ValidationResult (Maybe EndpointHttpConfig)
validateHttpConfig (Just (EndpointHttpEntry headers)) =
  pure $ EndpointHttpConfig <$> (traverse validateHeader) <$> headers
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

validateThemes :: [ThemeEntry] -> FilePath -> ValidationResult [ThemeConfig]
validateThemes [] _ = failure MissingThemes
validateThemes themes configFilePath =
  case (validateAll collectionValidations themes) of
    (Success _) -> traverse (validateTheme configFilePath) themes
    (Failure e) -> Failure e
  where
    collectionValidations =
      [validateUniqueThemeNames, validateExactlyOneDefaultTheme]

validateUniqueThemeNames :: [ThemeEntry] -> ValidationResult ()
validateUniqueThemeNames themes = case findDuplicates of
  [] -> pure () -- (traverse validateEndpoint endpoints)
  duplicates -> failure (DuplicateThemeNames duplicates)
  where
    findDuplicates = names \\ (nub names)
    names = map (\(ThemeEntry name _ _) -> name) themes

validateExactlyOneDefaultTheme ::
  [ThemeEntry] -> Validation (NonEmpty ValidationError) ()
validateExactlyOneDefaultTheme themes =
  case Relude.filter isDefaultTheme themes of
    [] -> failure MissingDefaultTheme
    [_] -> pure ()
    tooMany -> failure (MultipleDefaultThemes (map themeName tooMany))

themeName :: ThemeEntry -> Text
themeName (ThemeEntry name _ _) = name

isDefaultTheme :: ThemeEntry -> Bool
isDefaultTheme (ThemeEntry _ defaultFlag _) = defaultFlag

validateTheme :: FilePath -> ThemeEntry -> ValidationResult ThemeConfig
validateTheme configFilePath (ThemeEntry name defaultFlag themeFilePath) =
  ThemeConfig <$> validateName <*> pure defaultFlag <*> validateThemePath
  where
    validateName = name <$ failureIf (Text.null name) EmptyName
    validateThemePath = pure $ createThemePath configFilePath themeFilePath

validateDefaultTheme :: [ThemeEntry] -> FilePath -> ValidationResult ThemeConfig
validateDefaultTheme [] _ = failure MissingThemes
validateDefaultTheme themes configFilePath =
  case List.find isDefaultTheme themes of
    (Just theme) -> validateTheme configFilePath theme
    Nothing -> failure MissingDefaultTheme

createThemePath :: FilePath -> FilePath -> FilePath
createThemePath configFilePath themeFilePath
  | isRelative themeFilePath =
    (takeDirectory configFilePath) </> (normalise themeFilePath)
  | otherwise =
    normalise themeFilePath
