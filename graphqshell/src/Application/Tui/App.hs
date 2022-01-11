module Application.Tui.App where

import Application.Tui.Screen
import Application.Tui.Types
import Brick
import Optics
import Optics.TH
import Relude

brickApplication :: CommandChannel -> Theme -> App TuiState TuiEvent WidgetName
brickApplication actionChan theme =
  App
    { appDraw = render,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvents actionChan,
      appStartEvent = pure,
      appAttrMap = const (themeToAttrMap theme)
    }

render :: TuiState -> [Widget WidgetName]
render s = vBox . fmap (renderWidget s (focusedViewName s)) <$> visibleViewWidgets s

visibleViewWidgets :: TuiState -> [[WidgetName]]
visibleViewWidgets s = visibleWidgets focusedView
  where
    defaultView = view (tsConfig % tcDefaultView) s
    focusedView = fromMaybe defaultView $ focusGetCurrent $ (view tsViews % vsFocusedView) s
