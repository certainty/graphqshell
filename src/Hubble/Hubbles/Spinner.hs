{-# LANGUAGE TemplateHaskell #-}

module Hubble.Hubbles.Spinner where

import Brick (Widget, txt)
import Data.Vector
import Hubble.Internal.Types (Model)
import Hubble.Program (Message (TickMsg), UpdateM, cont, mkModel)
import Lens.Micro.Platform (ix, makeLenses, (%~), (^.))
import Relude hiding (fromList, length, null, state)

data SpinnerState = SpinnerState
  { _spsGlyphs :: Vector Text,
    _spsGlyphIndex :: Int
  }

makeLenses ''SpinnerState

type SpinnerModel cmd msg = Model SpinnerState cmd msg

newModel :: [Text] -> SpinnerModel cmd msg
newModel glyphs = mkModel (SpinnerState (fromList glyphs) 0) mempty spinnerUpdate spinnerView

spinnerUpdate :: SpinnerState -> Message msg -> UpdateM (SpinnerState, [cmd])
spinnerUpdate state TickMsg
  | null (state ^. spsGlyphs) = cont state
  | otherwise = cont $ state & spsGlyphIndex %~ newIndex
  where
    newIndex idx = (idx + 1) `mod` (length (state ^. spsGlyphs) - 1)
spinnerUpdate s _ = cont s

spinnerView :: SpinnerState -> Widget ()
spinnerView state = txt (state ^. spsGlyphs . ix (state ^. spsGlyphIndex))
