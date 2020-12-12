-- | Validions for the application configuration

module Shell.Configuration.Validation (
  validateConfiguration,
  ValidationError(..)
  ) where
import Relude
import Validation
import Data.List ((\\), nub)
import Lens.Micro.Platform ((^.))
import Shell.Configuration.Types

data ValidationError =
    MultipleDefaultEndpoints [Text] 
  | MultipleDefaultThemes [Text]
  | DuplicateNames [Text]
  | MissingDefaultEndpoint
  | MissingDefaultTheme
  deriving (Eq, Show)


validateConfiguration ::  ApplicationConfig -> Validation (NonEmpty ValidationError) ApplicationConfig
validateConfiguration = validateAll [
  validateSingleDefaultEndpoint,
  validateSingleDefaultTheme,
  validateUniqueEndpointNames,
  validateUniqueThemeNames
  ]
  where
    validateSingleDefaultEndpoint cfg = do
      case Relude.map _endpointName (Relude.filter _endpointIsDefault (cfg ^. appConfigEndpoints)) of
        [ ]       -> failure MissingDefaultEndpoint
        [_]       -> Success cfg
        endpoints -> failure (MultipleDefaultEndpoints endpoints)
        
    validateSingleDefaultTheme cfg = do
      case Relude.map _themeName (Relude.filter _themeIsDefault (cfg ^. appConfigThemes)) of
        [ ]    -> failure MissingDefaultTheme
        [_]    -> Success cfg
        themes -> failure (MultipleDefaultThemes themes)

    validateUniqueEndpointNames cfg = do
      let duplicates = findDuplicates $ Relude.map _endpointName (cfg ^. appConfigEndpoints)
      if Relude.null duplicates then
        Success cfg
      else failure (DuplicateNames duplicates)

    validateUniqueThemeNames cfg = do
      let duplicates = findDuplicates $ Relude.map _themeName (cfg ^. appConfigThemes)
      if Relude.null duplicates then
        Success cfg
      else failure (DuplicateNames duplicates)

    findDuplicates :: (Eq a) => [a] -> [a]
    findDuplicates ls = ls \\ (nub ls) 


