module Shell.Components.Introspection where

import Relude
import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Graphics.Vty as V


view :: Widget ()
view = padBottom Max $ fieldView <+> vBorder <+> typeView

typeView :: Widget ()
typeView =  vLimitPercent 70 $ padRight Max (str "typeinfo")

fieldView :: Widget ()
fieldView = vLimitPercent 30 (str "fields")
