{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module GQShell.Application.TUI.Activities.Main.EndpointMenu where

import Brick
import Brick.Widgets.Border (border, borderAttr, hBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import Control.Exception.Safe (MonadThrow)
import GQShell.Application.Config
import GQShell.Application.TUI.Shared (AppCommand (ConnectEndpoint), Command', Focus, Focusable (focusL, hasFocus), Message', Model')
import qualified GQShell.Application.TUI.Style as Style
import Hubble.KeyMap
import Hubble.Program (Message (KeyMsg), UpdateM, cont, logInfo, mState, mkModel, performBatch)
import Lens.Micro.Platform hiding (view)
import qualified Lens.Micro.Platform as Lens
import Relude

data Endpoint = Endpoint
  { _epName :: Text,
    _epConfig :: EndpointConfig
  }
  deriving (Show)

makeLenses ''Endpoint

-- Selection management
data MenuEntry = MenuEntry
  { _meEndpoint :: Endpoint
  }
  deriving (Show)

makeLenses ''MenuEntry

-- The keymap

data MenuKeys = MenuKeys
  { _mkUp :: Binding,
    _mkDown :: Binding,
    _mkAccept :: Binding
  }
  deriving (Show)

makeLenses ''MenuKeys

mkMenuKeys :: MonadThrow m => m MenuKeys
mkMenuKeys =
  MenuKeys
    <$> mkBinding Enabled ["k", "<up>"] (withHelp "k/↑" "Prev")
    <*> mkBinding Enabled ["j", "<down>"] (withHelp "j/↓" "Next")
    <*> mkBinding Enabled ["<return>"] (withHelp "ret" "Select")

-- The hubble state
data EndpointMenuState = EndpointMenuState
  { _emsEntries :: [MenuEntry],
    _emsFocus :: Focus,
    _emsSelectedEntryIdx :: Int,
    _emsMaxIndex :: Int,
    _emsKeyMap :: MenuKeys
  }
  deriving (Show)

makeLenses ''EndpointMenuState

type EndpointMenuModel = Model' EndpointMenuState

instance Focusable EndpointMenuState where
  focusL = emsFocus

newModel :: (MonadThrow m) => Focus -> [EndpointConfig] -> m EndpointMenuModel
newModel f endpoints = do
  km <- mkMenuKeys
  pure $ mkModel (initialState km) [] update view
  where
    initialState km = selectFirst (EndpointMenuState menuEntries f 0 maxIdx km)
    menuEntries = MenuEntry <$> map makeEndpoint endpoints
    maxIdx = max 0 (length menuEntries)

makeEndpoint :: EndpointConfig -> Endpoint
makeEndpoint cfg =
  Endpoint
    (cfg ^. endpointName)
    cfg

keyBindings :: EndpointMenuModel -> [Binding]
keyBindings model = [menuKeys ^. mkUp, menuKeys ^. mkDown, menuKeys ^. mkAccept]
  where
    menuKeys = model ^. mState . emsKeyMap

update :: EndpointMenuState -> Message' -> UpdateM (EndpointMenuState, [Command'])
update s (KeyMsg k)
  | (not . hasFocus) s = cont s
  | (km ^. mkUp) `matches` k = cont (selectNext s)
  | (km ^. mkDown) `matches` k = cont (selectPrev s)
  | (km ^. mkAccept) `matches` k = logInfo "selecting endpoint" >> performBatch s (ConnectEndpoint <$> catMaybes [selectedEndpointConfig s])
  | otherwise = logInfo "Menu: no matching key" >> cont s
  where
    km = s ^. emsKeyMap
update s _ = cont s

selectedEndpointConfig :: EndpointMenuState -> Maybe EndpointConfig
selectedEndpointConfig s = (s ^. emsEntries) ^? Lens.ix (s ^. emsSelectedEntryIdx) . meEndpoint . epConfig

selectFirst :: EndpointMenuState -> EndpointMenuState
selectFirst = modAddMenuIndex 0

selectNext :: EndpointMenuState -> EndpointMenuState
selectNext = modAddMenuIndex 1

selectPrev :: EndpointMenuState -> EndpointMenuState
selectPrev = modAddMenuIndex (-1)

modAddMenuIndex :: Int -> EndpointMenuState -> EndpointMenuState
modAddMenuIndex i s = s & emsSelectedEntryIdx %~ (\j -> (j + i) `mod` s ^. emsMaxIndex)

view :: EndpointMenuState -> Widget ()
view s =
  overrideAttr borderAttr borderStyle $
    joinBorders $
      withBorderStyle unicodeRounded $
        border $
          vBox $ titleView s : hBorder : entriesView s
  where
    borderStyle = if hasFocus s then Style.activeBorder else Style.inactiveBorder

titleView :: EndpointMenuState -> Widget ()
titleView _ = withAttr Style.activeTabCaption $ padLeft (Pad 1) $ txt "Endpoints"

entriesView :: EndpointMenuState -> [Widget ()]
entriesView s
  | null entries = [txt "No endpoints"]
  | otherwise = viewEntry s <$> zip [0 ..] entries
  where
    entries = s ^. emsEntries

viewEntry :: EndpointMenuState -> (Int, MenuEntry) -> Widget ()
viewEntry s (idx, entry)
  | idx == selectedIndex = withAttr (Style.menuItem <> Style._active) $ menuEntryView True entry
  | otherwise = withAttr Style.menuItem $ menuEntryView False entry
  where
    selectedIndex = s ^. emsSelectedEntryIdx

menuEntryView :: Bool -> MenuEntry -> Widget ()
menuEntryView isSelected e =
  padLeft (Pad 1) $
    padRight (Pad 3) $
      hBox [selectionIndicator, txt $ e ^. meEndpoint . epName]
  where
    selectionIndicator :: Widget ()
    selectionIndicator = padRight (Pad 1) $ if isSelected then txt ">" else txt " "
