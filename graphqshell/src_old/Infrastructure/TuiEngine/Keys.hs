module Infrastructure.TuiEngine.Keys (Key (..), fromVTYKey) where

import qualified Graphics.Vty as V
import Relude (Maybe (Just, Nothing))
import Prelude (Char, Eq, Show)

data Key
  = Key Char
  | KeyCtrl Char
  | KeyAlt Char
  | KeyMeta Char
  | KeyShift Char
  | KeyBackspace
  | KeyEnter
  | KeyTab
  | KeyEscape
  | KeySpace
  | KeyPageUp
  | KeyPageDown
  | KeyHome
  | KeyEnd
  | KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeyInsert
  | KeyDelete
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | FKey10
  | KeyF11
  | KeyF12
  deriving (Eq, Show)

fromVTYKey :: V.Key -> [V.Modifier] -> Maybe Key
fromVTYKey (V.KChar c) [V.MShift] = Just (KeyShift c)
fromVTYKey (V.KChar c) [V.MCtrl] = Just (KeyCtrl c)
fromVTYKey (V.KChar c) [V.MAlt] = Just (KeyAlt c)
fromVTYKey (V.KChar c) [V.MMeta] = Just (KeyMeta c)
fromVTYKey (V.KChar c) [] = Just (Key c)
fromVTYKey V.KBackTab _ = Just KeyBackspace
fromVTYKey V.KBS _ = Just KeyBackspace
fromVTYKey V.KEnter _ = Just KeyEnter
fromVTYKey V.KEsc _ = Just KeyEscape
fromVTYKey V.KHome _ = Just KeyHome
fromVTYKey V.KLeft _ = Just KeyLeft
fromVTYKey V.KRight _ = Just KeyRight
fromVTYKey V.KUp _ = Just KeyUp
fromVTYKey V.KDown _ = Just KeyDown
fromVTYKey V.KIns _ = Just KeyInsert
fromVTYKey V.KDel _ = Just KeyDelete
fromVTYKey V.KPageUp _ = Just KeyPageUp
fromVTYKey V.KPageDown _ = Just KeyPageDown
fromVTYKey (V.KFun 1) _ = Just KeyF1
fromVTYKey (V.KFun 2) _ = Just KeyF2
fromVTYKey (V.KFun 3) _ = Just KeyF3
fromVTYKey (V.KFun 4) _ = Just KeyF4
fromVTYKey (V.KFun 5) _ = Just KeyF5
fromVTYKey (V.KFun 6) _ = Just KeyF6
fromVTYKey (V.KFun 7) _ = Just KeyF7
fromVTYKey (V.KFun 8) _ = Just KeyF8
fromVTYKey (V.KFun 9) _ = Just KeyF9
fromVTYKey (V.KFun 10) _ = Just FKey10
fromVTYKey (V.KFun 11) _ = Just KeyF11
fromVTYKey (V.KFun 12) _ = Just KeyF12
fromVTYKey _ _ = Nothing
