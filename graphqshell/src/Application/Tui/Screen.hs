{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Application.Tui.Screen where

import Brick.Focus (FocusRing, focusGetCurrent, focusSetCurrent)
import qualified Data.Vector as V
import Optics
import Relude hiding (state)

data ViewName = ViewName deriving (Eq, Show, Hashable, Generic)

data WidgetName = WidgetName deriving (Eq, Show, Hashable, Generic)

data Visibility = Hidden | Visible deriving (Eq, Show)

data Tile = Tile
  { _tileVisibility :: Visibility,
    _tileName :: WidgetName
  }
  deriving (Eq, Show)

makeLenses ''Tile

newtype Layer = Layer
  { _layerTiles :: (V.Vector Tile)
  }
  deriving (Eq, Show)

makeLenses ''Layer

type Layers = V.Vector Layer

type instance Index Layer = WidgetName

type instance IxValue Layer = Tile

instance Ixed Layer where
  ix = tile

tile :: WidgetName -> AffineTraversal' Layer Tile
tile name = atraversalVL $ \point f layer ->
  case V.findIndex (== name) (tileNames layer) of
    Just idx -> (f (V.! idx tileNames)) <&> \new -> (Layer (V.// (allTiles layer) [(idx, new)]))
    Nothing -> point layer
  where
    tileNames layer = V.map (^. tileName) (allTiles layer)
    allTiles = view layerTiles

layeriso :: Iso' Layer (V.Vector Tile)
layeriso = iso from to
  where
    from (Layer xs) = xs
    to = Layer

data View = View
  { _vFocus :: WidgetName,
    _vLayers :: Layers
  }
  deriving (Eq, Show)

makeLenses ''View

data ViewState = ViewState
  { _vsViews :: Map ViewName View,
    _vsFocusedView :: FocusRing ViewName
  }

makeLenses ''ViewState

-- visibleWidgets :: ViewState -> ViewName -> [[WidgetName]]
