{-# LANGUAGE TemplateHaskell #-}

-- | The main component of the application.
-- Use this as the entry point to understand how the app works.
module Shell.Components.Main
  ( update
  , view
  , State
  , Event(..)
  , initialState
  )
where

import           Brick
import qualified Brick.Focus                   as Focus
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import qualified GraphQL.API                   as API
import           GraphQL.Introspection.Schema                                           ( GraphQLType
                                                                                          ( Object
                                                                                          )
                                                                                        , Schema
                                                                                        , query
                                                                                        )
import qualified Graphics.Vty                  as V
import           Lens.Micro.Platform                                                    ( makeLenses
                                                                                        , (^.)
                                                                                        )
import           Relude                                                          hiding ( State
                                                                                        , state
                                                                                        )
import qualified Shell.Components.Introspector as Intro
import           Shell.Components.Types
import           Shell.Components.Utilities
import           Text.URI                                                               ( renderStr
                                                                                        )

data Event
  = IntrospectorEvent Intro.Event
  | Tick
  deriving (Eq, Ord, Show)

{-
  ____  _        _
 / ___|| |_ __ _| |_ ___
 \___ \| __/ _` | __/ _ \
  ___) | || (_| | ||  __/
 |____/ \__\__,_|\__\___|

-}

-- | The application state holds global data and component specific data.
-- The application will delegate updates to the currently active component automatically.
data State = State
  { -- | The 'Schema' of the currently connected GraphQL API
    _stSchema            :: Schema
  ,
    -- | settings for the 'API' client
    _stApiSettings       :: API.ApiSettings
  ,
    -- | Manage which component has the focus
    _stFocus             :: !(Focus.FocusRing ComponentName)
  ,
    -- | State for the introspector component
    _stIntrospectorState :: Intro.State
  }

makeLenses ''State

{-
  ___       _ _
 |_ _|_ __ (_) |_
  | || '_ \| | __|
  | || | | | | |_
 |___|_| |_|_|\__|

-}


initialState :: API.ApiSettings -> Schema -> State
initialState settings schema = State
  schema
  settings
  (Focus.focusRing components)
  (Intro.initialState schema (Object (query schema)))
  where components = [()]

{-
  _   _           _       _
 | | | |_ __   __| | __ _| |_ ___
 | | | | '_ \ / _` |/ _` | __/ _ \
 | |_| | |_) | (_| | (_| | ||  __/
  \___/| .__/ \__,_|\__,_|\__\___|
       |_|

-}

update :: State -> BrickEvent ComponentName Event -> EventM ComponentName (Next State)
update s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt s
update s (VtyEvent evt) =
  updateComponent s stIntrospectorState Intro.update (VtyEvent evt)
update s (AppEvent (IntrospectorEvent event)) =
  updateComponent s stIntrospectorState Intro.update (AppEvent event)
update s _ = continue s

{-
 __     ___
 \ \   / (_) _____      __
  \ \ / /| |/ _ \ \ /\ / /
   \ V / | |  __/\ V  V /
    \_/  |_|\___| \_/\_/

-}

view :: State -> [Widget ComponentName]
view state = [mainWidget state]

mainWidget :: State -> Widget ComponentName
mainWidget state = withBorderStyle unicodeRounded $ joinBorders $ mainViewPort state

-- TODO: extract into component
topBar :: State -> Widget ComponentName
topBar state = hBox [padRight Max $ padLeft (Pad 1) $ txt (toText url)]
  where url = renderStr . API.apiURI $ state ^. stApiSettings

mainViewPort :: State -> Widget ComponentName
mainViewPort state =
  border
    $   topBar state
    <=> hBorder
    <=> tabLine
    <=> hBorder
    <=> Intro.view (state ^. stIntrospectorState)
    <=> hBorder
    <=> statusLine

-- TODO: extract into component
tabLine :: Widget ComponentName
tabLine = hBox [padRight Max $ padLeft (Pad 1) $ txt "[ Introspector ] | [ Query ]"]

-- TODO: extract into component
statusLine :: Widget ComponentName
statusLine = hBox [padRight Max $ padLeft (Pad 1) $ str "C-c: Exit"]
