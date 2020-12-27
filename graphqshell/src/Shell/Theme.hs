module Shell.Theme where
import           Relude
import qualified Graphics.Vty                  as V
import           Shell.Configuration
import           Brick.Themes
import qualified Shell.Components.Introspector as Intro
import           Control.Exception.Safe                                                 ( MonadThrow
                                                                                        , throw
                                                                                        )

import           Lens.Micro.Platform                                                    ( (^.)
                                                                                        )

data ThemeError = ThemeLoadError Text
  deriving Show
instance Exception ThemeError

-- All attributes will be collected here
defaultTheme :: Theme
defaultTheme = newTheme V.defAttr allAttributes where allAttributes = Intro.attributes

loadTheme :: (MonadThrow m, MonadIO m) => ThemeConfig -> m Theme
loadTheme config = do
  result <- liftIO $ loadCustomizations (config ^. themePath) defaultTheme
  case result of
    (Left  e    ) -> throw (ThemeLoadError (toText e))
    (Right theme) -> pure theme
