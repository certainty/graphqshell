module Shell.Theme where

import Brick.Themes
import Control.Exception.Safe
  ( MonadThrow,
    throw,
  )
import qualified Graphics.Vty as V
import Lens.Micro.Platform
  ( (^.),
  )
import Relude
import qualified Shell.Components.CommandBar as CommandBar
import qualified Shell.Components.Introspector as Intro
import qualified Shell.Components.Main as Main
import Shell.Configuration

data ThemeError = ThemeLoadError Text
  deriving (Show)

instance Exception ThemeError

-- All attributes will be collected here
defaultTheme :: Theme
defaultTheme = newTheme V.defAttr allAttributes
  where
    allAttributes = Intro.attributes ++ CommandBar.attributes ++ Main.attributes

loadTheme :: (MonadThrow m, MonadIO m) => ThemeConfig -> m Theme
loadTheme config = do
  result <- liftIO $ loadCustomizations (config ^. themePath) defaultTheme
  case result of
    (Left e) -> throw (ThemeLoadError (toText e))
    (Right theme) -> pure theme
