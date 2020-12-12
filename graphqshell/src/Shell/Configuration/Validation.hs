{-# LANGUAGE RankNTypes #-}
-- | Validions for the application configuration

module Shell.Configuration.Validation
  ( validateConfiguration
  , ValidationError(..)
  )
where
import           Relude
import           Validation
import           Data.List                      ( (\\)
                                                , nub
                                                )
import           Lens.Micro.Platform            ( (^.)
                                                , (^..)
                                                , each
                                                , Lens'
                                                )
import           Shell.Configuration.Types

data ValidationError =
    MultipleDefaultEndpoints [Text]
  | MultipleDefaultThemes [Text]
  | DuplicateNames [Text]
  | MissingDefaultEndpoint
  | MissingDefaultTheme
  deriving (Eq, Show)

validateConfiguration
  :: ApplicationConfig
  -> Validation (NonEmpty ValidationError) ApplicationConfig
validateConfiguration = validateAll activeValidations
 where
  activeValidations =
    [ validateSingleDefaultEndpoint
    , validateSingleDefaultTheme
    , validateUniqueEndpointNames
    , validateUniqueThemeNames
    ]

validateSingleDefaultEndpoint
  :: ApplicationConfig
  -> Validation (NonEmpty ValidationError) ApplicationConfig
validateSingleDefaultEndpoint cfg = do
  case namesForDefaults endpointName (cfg ^. appConfigEndpoints) of
    []        -> failure MissingDefaultEndpoint
    [_]       -> Success cfg
    endpoints -> failure (MultipleDefaultEndpoints endpoints)

validateSingleDefaultTheme
  :: ApplicationConfig
  -> Validation (NonEmpty ValidationError) ApplicationConfig
validateSingleDefaultTheme cfg = do
  case namesForDefaults themeName (cfg ^. appConfigThemes) of
    []     -> failure MissingDefaultTheme
    [_]    -> Success cfg
    themes -> failure (MultipleDefaultThemes themes)


namesForDefaults :: (IsDefaultConfig b) => (Lens' b Text) -> [b] -> [Text]
namesForDefaults getName cfg = (Relude.filter isDefault cfg) ^.. each . getName

validateUniqueEndpointNames
  :: ApplicationConfig
  -> Validation (NonEmpty ValidationError) ApplicationConfig
validateUniqueEndpointNames =
  validateNoDuplicateNames appConfigEndpoints endpointName

validateUniqueThemeNames
  :: ApplicationConfig
  -> Validation (NonEmpty ValidationError) ApplicationConfig
validateUniqueThemeNames = validateNoDuplicateNames appConfigThemes themeName

validateNoDuplicateNames
  :: Lens' ApplicationConfig [b]
  -> (Lens' b Text)
  -> ApplicationConfig
  -> Validation (NonEmpty ValidationError) ApplicationConfig
validateNoDuplicateNames getItems getName cfg = do
  case findDuplicates of
    []         -> pure cfg
    duplicates -> failure (DuplicateNames duplicates)
 where
  findDuplicates = names \\ (nub names)
  names          = cfg ^. getItems ^.. each . getName
