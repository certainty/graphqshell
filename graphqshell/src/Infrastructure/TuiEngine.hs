{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Infrastructure.TuiEngine where

import Brick
import qualified Brick.BChan as Bch
import Brick.Focus (FocusRing, focusGetCurrent, focusSetCurrent)
import Brick.Themes (Theme, themeToAttrMap)
import Control.Monad.Logger (MonadLogger, WriterLoggingT, runWriterLoggingT)
import qualified Data.Vector as V
import Optics
import Optics.TH
import Relude hiding (state)

data ViewName = ViewName deriving (Eq, Show)

data Name = Name deriving (Eq, Show)

data ViewState = Hidden | Visible deriving (Eq, Show)

data Tile = Tile ViewState Name deriving (Eq, Show)

newtype Layer = Layer (V.Vector Tile) deriving (Eq, Show)

type Layers = V.Vector Layer

data View = View
  { _vFocus :: Name,
    _vLayers :: Layers
  }

makeLenses ''View

data ViewSettings = ViewSettings
  { _vsViews :: Map ViewName View,
    _vsFocusedView :: FocusRing ViewName
  }

makeLenses ''ViewSettings

data AppState = AppState
  { _asDefaultView :: ViewName,
    _asViews :: ViewSettings,
    _asFocusedViewName :: ViewName
  }

makeLenses ''AppState

focusedViewName :: AppState -> ViewName
focusedViewName s = fromMaybe (view s asDefaultView) $ focusGetCurrent $ view (asViews % vsFocusedView) s

data AppEvent = AppEvent

data AppAction = Action
  { _eaDescription :: Text,
    _eaHandler :: AppState -> IO (Maybe AppEvent)
  }

makeLenses ''AppAction

data AppEventHandlerState = EventHandlerState
  { _ehsAppState :: AppState,
    _ehsActions :: [AppAction]
  }

makeLenses ''AppEventHandlerState

newtype AppEventHandler a = EventHandler {runEventHandler :: StateT AppEventHandlerState (WriterLoggingT Identity) a}
  deriving (Functor, Applicative, Monad, MonadState AppEventHandlerState, MonadLogger)

type ActionChannel = Bch.BChan AppAction

type EventChannel = Bch.BChan AppEvent

brickApplication :: ActionChannel -> Theme -> App AppState AppEvent Name
brickApplication actionChan theme =
  App
    { appDraw = renderVisible,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvents actionChan,
      appStartEvent = pure,
      appAttrMap = const (themeToAttrMap theme)
    }

renderVisible :: AppState -> [Widget Name]
renderVisible renderView state = vBox . fmap (renderView state (vsFocusedView state)) <$> visibleViews state

renderView :: AppState -> ViewName -> Name -> Widget Name
renderView _ _ = str "Hello World"

handleEvents :: ActionChannel -> AppEvent -> AppState -> EventM Name (Next AppState)
handleEvents actionChan event appState = do
  let eventHandlerState = EventHandlerState appState []
  (nextState, _eventHandlers) <- runWriterLoggingT (runStateT (runEventHandler (handleEvent event)) eventHandlerState)
  let actions = ehsActions nextState
  for_ actions $ Bch.writeBChan actionChan
  pure $ Next nextState

handleEvent :: AppEvent -> AppEventHandler a0
handleEvent = error "not implemented"
