{-# LANGUAGE TemplateHaskell #-}

module GQShell.Application.TUI.Activities.Main.Tabs where

import Brick (Widget, fill, hBox, hLimit, joinBorders, overrideAttr, padAll, vBox, vLimit, withAttr, withBorderStyle)
import Brick.AttrMap (AttrName)
import Brick.Widgets.Border (borderAttr, borderElem, hBorder, vBorder)
import Brick.Widgets.Border.Style (BorderStyle (bsCornerBL, bsCornerBR, bsCornerTR), bsCornerTL, unicode, unicodeRounded)
import Brick.Widgets.Center
import Brick.Widgets.Core (txt)
import Control.Exception.Safe (MonadThrow)
import qualified Data.List
import qualified Data.Text as T
import Data.Vector (Vector, foldl)
import GQShell.Application.TUI.Activities.Introspector (IntrospectorModel)
import qualified GQShell.Application.TUI.Activities.Introspector as Intro
import GQShell.Application.TUI.Activities.Query (QueryModel)
import qualified GQShell.Application.TUI.Activities.Query as Query
import GQShell.Application.TUI.Activities.Summary (SummaryModel)
import qualified GQShell.Application.TUI.Activities.Summary as Summary
import GQShell.Application.TUI.Shared (AppMessage (EndpointChange), Command', EndpointState (Connected), Focus (Focus, Unfocus), Focusable (..), Message', Model', Tile, hasFocus, makeTile, updateTile, viewTile)
import qualified GQShell.Application.TUI.Style as Style
import Hubble.KeyMap (Binding, BindingState (Enabled), matches, mkBinding, withHelp)
import Hubble.Program (Message (AppMsg, KeyMsg), UpdateM, cont, logInfo, logWarning, mState, mkModel)
import Lens.Micro.Platform (ix, makeLenses, (.~), (^.))
import Relude hiding (First, Last)

data TabLabelPosition = First | Last | Middle deriving (Eq, Show)

data TabLabel = TabLabel
  { _tlCaption :: Text,
    _tlFocus :: Focus,
    _tlPosition :: TabLabelPosition
  }
  deriving (Eq, Show)

data TabKeys = TabKeys
  { _tkNextTab :: Binding,
    _tkPrevTab :: Binding
  }
  deriving (Eq, Show)

makeLenses ''TabKeys

mkTabKeys :: (MonadThrow m) => m TabKeys
mkTabKeys =
  TabKeys
    <$> mkBinding Enabled ["<tab>"] (withHelp "<tab>" "Next tab")
    <*> mkBinding Enabled ["S-<tab>"] (withHelp "S-<tab>" "Previous tab")

data TabState = TabState
  { _tsSummary :: Tile SummaryModel,
    _tsIntrospector :: Tile IntrospectorModel,
    _tsQuery :: Tile QueryModel,
    _tsLabels :: Vector TabLabel,
    _tsFocus :: Focus,
    _tsKeyMap :: TabKeys
  }

makeLenses ''TabState

instance Focusable TabState where
  focusL = tsFocus

makeLenses ''TabLabel

instance Focusable TabLabel where
  focusL = tlFocus

type TabModel = Model' TabState

tabKeyBindings :: TabKeys -> [Binding]
tabKeyBindings tk = [tk ^. tkNextTab, tk ^. tkPrevTab]

newModel :: (MonadThrow m) => m TabModel
newModel = do
  keys <- mkTabKeys
  pure $ mkModel (TabState (makeTile sm) (makeTile im) (makeTile qm) labels Unfocus keys) mempty update view
  where
    labels = fromList [TabLabel "Summary" Unfocus First, TabLabel "Introspector" Unfocus Middle, TabLabel "Query" Unfocus Last]
    sm = Summary.newModel Focus
    im = Intro.newModel Unfocus
    qm = Query.newModel Unfocus

keyBindings :: TabModel -> [Binding]
keyBindings tabModel
  | hasFocus (tabState ^. tsSummary) = tabKeys <> []
  | hasFocus (tabState ^. tsQuery) = tabKeys <> []
  | hasFocus (tabState ^. tsIntrospector) = tabKeys <> []
  | otherwise = tabKeys
  where
    tabState = tabModel ^. mState
    tabKeys = tabKeyBindings $ tabState ^. tsKeyMap

update :: TabState -> Message' -> UpdateM (TabState, [Command'])
update s msg@(AppMsg (EndpointChange (Connected _ _))) = do
  let s' = switchToSummary s
  relayUpdate s' msg
update s keyMsg@(KeyMsg k)
  | _tkNextTab keyMap `matches` k = logWarning ("Next tab" <> show (s ^. tsLabels)) >> cont (focusNextTab s)
  | _tkPrevTab keyMap `matches` k = logWarning "Prev tab" >> cont (focusPrevTab s)
  | otherwise = relayUpdate s keyMsg
  where
    keyMap = s ^. tsKeyMap
update s msg = relayUpdate s msg

relayUpdate :: TabState -> Message' -> UpdateM (TabState, [Command'])
relayUpdate s msg = do
  (sm', smCmds) <- updateTile (_tsSummary s) msg
  (im', imCmds) <- updateTile (_tsIntrospector s) msg
  (qm', qmCmds) <- updateTile (_tsQuery s) msg
  pure (s {_tsSummary = sm', _tsIntrospector = im', _tsQuery = qm'}, smCmds <> imCmds <> qmCmds)

focusNextTab :: TabState -> TabState
focusNextTab s
  | hasFocus (s ^. tsSummary) = switchToIntrospector s
  | hasFocus (s ^. tsIntrospector) = switchToQuery s
  | otherwise = switchToSummary s

focusPrevTab :: TabState -> TabState
focusPrevTab s
  | hasFocus (s ^. tsSummary) = switchToQuery s
  | hasFocus (s ^. tsQuery) = switchToIntrospector s
  | otherwise = switchToSummary s

switchToSummary :: TabState -> TabState
switchToSummary s =
  let s' = unfocusAllLabels s
      s'' = s & tsLabels . ix 0 . tlFocus .~ Focus
   in s''
        { _tsSummary = focusOn (s' ^. tsSummary),
          _tsIntrospector = focusOff (s' ^. tsIntrospector),
          _tsQuery = focusOff (s' ^. tsQuery)
        }

switchToIntrospector :: TabState -> TabState
switchToIntrospector s =
  let s' = unfocusAllLabels s
      s'' = s & tsLabels . ix 1 . tlFocus .~ Focus
   in s''
        { _tsSummary = focusOff (s' ^. tsSummary),
          _tsIntrospector = focusOn (s' ^. tsIntrospector),
          _tsQuery = focusOff (s' ^. tsQuery)
        }

switchToQuery :: TabState -> TabState
switchToQuery s =
  let s' = unfocusAllLabels s
      s'' = s' & tsLabels . ix 2 . tlFocus .~ Focus
   in s''
        { _tsSummary = focusOff (s' ^. tsSummary),
          _tsIntrospector = focusOff (s' ^. tsIntrospector),
          _tsQuery = focusOn (s' ^. tsQuery)
        }

unfocusAllLabels :: TabState -> TabState
unfocusAllLabels s = s {_tsLabels = fmap unfocus labels}
  where
    labels = s ^. tsLabels
    unfocus l = l & tlFocus .~ Unfocus

view :: TabState -> Widget ()
view s = vBox [viewTabLabels s, viewTabContent s]

viewTabLabels :: TabState -> Widget ()
viewTabLabels s =
  overrideAttr
    borderAttr
    (if hasFocus s then Style.activeBorder else Style.inactiveBorder)
    (viewLabels s)

viewLabels :: TabState -> Widget ()
viewLabels tabState =
  withBorderStyle Style.tabBorder $
    hBox $ fmap constrainLabel (viewTabLabel' <$> tabLabels) ++ [tabGap]
  where
    tabLabels = toList $ tabState ^. tsLabels
    constrainLabel = hLimit (maxLabelLength + 4)
    maxLabelLength = Data.List.maximum $ fmap (T.length . _tlCaption) tabLabels
    tabGap = vLimit 3 $ vBox [fill ' ', fill ' ', hBorder]

viewTabLabel' :: TabLabel -> Widget ()
viewTabLabel' (TabLabel caption focus First) = joinBorders $ vLimit 3 $ vBox [top, middle, bottom]
  where
    top = hBox [borderElem bsCornerTL, hBorder, borderElem bsCornerTR]
    middle = hBox [vBorder, hCenter $ withAttr (focusToAttrName focus) $ txt caption, vBorder]
    bottom = hBox [vBorder, fill ' ', borderElem bsCornerBR]
viewTabLabel' (TabLabel caption focus _) = joinBorders $ vLimit 3 $ vBox [top, middle, bottom]
  where
    top = hBox [borderElem bsCornerTL, hBorder, borderElem bsCornerTR]
    middle = hBox [vBorder, hCenter $ withAttr (focusToAttrName focus) $ txt caption, vBorder]
    bottom = hBox [borderElem bsCornerBL, if focus == Unfocus then hBorder else fill ' ', borderElem bsCornerBR]

focusToAttrName :: Focus -> AttrName
focusToAttrName Focus = Style.activeTabCaption
focusToAttrName Unfocus = Style.inactiveTabCaption

viewTabContent :: TabState -> Widget ()
viewTabContent s =
  maybe
    bugView
    wrapView
    ( viewTile (_tsSummary s)
        <|> viewTile (_tsIntrospector s)
        <|> viewTile (_tsQuery s)
    )
  where
    bugView = txt "BUG: no tab is visible"
    wrapView :: Widget () -> Widget ()
    wrapView w =
      overrideAttr borderAttr (if hasFocus s then Style.activeBorder else Style.inactiveBorder) $
        joinBorders $
          withBorderStyle unicodeRounded $
            vBox
              [ hBox [vBorder, padAll 2 w],
                hBox [borderElem bsCornerBL, hBorder]
              ]
