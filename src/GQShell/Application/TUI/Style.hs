{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant $" #-}
module GQShell.Application.TUI.Style where

import Brick hiding (attrMap)
import qualified Brick as B
import qualified Brick.Widgets.Border.Style as BS
import Graphics.Vty (dim)
import Graphics.Vty.Attributes (Color (ISOColor), bold, defAttr, rgbColor, withStyle)
import Hubble.Hubbles.Log (logError, logInfo, logLevel, logWarning)
import Relude
import Relude.Unsafe (read)

attrMap :: B.AttrMap
attrMap =
  B.attrMap
    defAttr
    [ (_subtle, fg $ rgbs "#383838"),
      (_highlight, fg $ rgbs "#7D56F4"),
      (_special, fg $ rgbs "#73F59F"),
      (_bold, withStyle defAttr bold),
      (_inactive, withStyle defAttr dim),
      (activeTabCaption, withStyle defAttr bold),
      (inactiveTabCaption, fg $ rgbs "#A9A9A9"),
      (activeBorder, fg $ ISOColor 62),
      (inactiveBorder, fg $ rgbs "#404040"),
      (menuItem <> _active, fg $ ISOColor 207),
      (menuTitle, bg $ ISOColor 62),
      (statusBar, rgbs "#C1C6B2" `Brick.on` rgbs "#353533"),
      (statusCaption, rgbs "#FFFDF5" `Brick.on` rgbs "#FF5F87"),
      (logLevel, withStyle defAttr bold),
      (logInfo, defAttr),
      (logError, fg $ ISOColor 230),
      (keyMapHelpKey, withStyle (withStyle defAttr dim) bold),
      (keyMapHelpDesc, withStyle defAttr dim),
      (logWarning, fg $ rgbs "#EEF739"),
      (tableTitle, withStyle defAttr bold)
    ]

-- General

_active :: AttrName
_active = attrName "active"

_inactive :: AttrName
_inactive = attrName "inactive"

_caption :: AttrName
_caption = attrName "caption"

_subtle :: AttrName
_subtle = attrName "subtle"

_special :: AttrName
_special = attrName "special"

_bold :: AttrName
_bold = attrName "bold"

_highlight :: AttrName
_highlight = attrName "highlight"

_border :: AttrName
_border = attrName "border"

activeBorder :: AttrName
activeBorder = _border <> _active

inactiveBorder :: AttrName
inactiveBorder = _border <> _inactive

-- Keymap Help
keyMapHelp :: AttrName
keyMapHelp = attrName "keymapHelp"

keyMapHelpKey :: AttrName
keyMapHelpKey = keyMapHelp <> attrName "key"

keyMapHelpDesc :: AttrName
keyMapHelpDesc = keyMapHelp <> attrName "desc"

-- Menu

menuItem :: AttrName
menuItem = attrName "menu" <> attrName "item"

menuTitle :: AttrName
menuTitle = attrName "menu" <> attrName "title"

-- Tabs

_tab :: AttrName
_tab = attrName "tab"

activeTabCaption :: AttrName
activeTabCaption = _tab <> _caption <> _active

inactiveTabCaption :: AttrName
inactiveTabCaption = _tab <> _caption <> _inactive

tabBorder :: BS.BorderStyle
tabBorder =
  BS.BorderStyle
    { BS.bsCornerTL = '╭',
      BS.bsCornerBR = '└',
      BS.bsCornerTR = '╮',
      BS.bsCornerBL = '┘',
      BS.bsIntersectFull = '┴',
      BS.bsIntersectL = '│',
      BS.bsIntersectR = '│',
      BS.bsIntersectT = '│',
      BS.bsIntersectB = '┴',
      BS.bsHorizontal = '─',
      BS.bsVertical = '│'
    }

-- Statusbar
_status :: AttrName
_status = attrName "status"

statusCaption :: AttrName
statusCaption = _status <> _caption

statusSpinner :: AttrName
statusSpinner = _status <> attrName "spinner"

statusBar :: AttrName
statusBar = _status

-- tables
tableTitle :: AttrName
tableTitle = attrName "table" <> attrName "title"

-- Utilities

rgb :: Int -> Int -> Int -> Color
rgb r g b = rgbColor r g b

-- Only used in this module so it's safe to be unsafe :D
rgbs :: String -> Color
rgbs ['#', r1, r2, g1, g2, b1, b2] =
  rgb
    (read ['0', 'x', r1, r2] :: Int)
    (read ['0', 'x', g1, g2] :: Int)
    (read ['0', 'x', b1, b2] :: Int)
rgbs _ = error "Invalid color"
