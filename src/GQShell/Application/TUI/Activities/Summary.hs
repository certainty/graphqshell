{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module GQShell.Application.TUI.Activities.Summary where

import Brick (Padding (Pad), Widget, padLeft, padRight, withAttr)
import Brick.Widgets.Core (txt)
import Brick.Widgets.Table (ColumnAlignment (..), Table, renderTable, setDefaultColAlignment, table)
import GQShell.Application.TUI.Shared
import qualified GQShell.Application.TUI.Style as Style
import GQShell.Core.GraphQL.Introspection.Statistics (Statistics, computeStatistics, statsInputTypeCardinality, statsOrphanedTypeCardinality, statsOutputTypeCardinality, statsTypeCardinality)
import Hubble.Program (Message (AppMsg), UpdateM, cont, mkModel)
import Lens.Micro.Platform hiding (view)
import Relude

data SummaryState = SummaryState
  { _susFocus :: Focus,
    _susStatistics :: Maybe Statistics
  }

makeLenses ''SummaryState

type SummaryModel = Model' SummaryState

instance Focusable SummaryState where
  focusL = susFocus

newModel :: Focus -> SummaryModel
newModel f = mkModel (SummaryState f Nothing) mempty update view

view :: SummaryState -> Widget ()
view (SummaryState Focus (Just stats)) = renderTable (cardinalityTable stats)
view _ = txt "No schema loaded"

cardinalityTable :: Statistics -> Table ()
cardinalityTable statistics =
  setDefaultColAlignment AlignCenter $
    table
      [ [title "# Types", title "# Input Types", title "# Output Types", title "# Orphaned Types"],
        [value statsTypeCardinality, value statsInputTypeCardinality, value statsOutputTypeCardinality, value statsOrphanedTypeCardinality]
      ]
  where
    title t = padLeft (Pad 2) $ padRight (Pad 2) $ withAttr Style.tableTitle $ txt t
    value :: Lens' Statistics Int -> Widget ()
    value getter = txt $ show $ statistics ^. getter

update :: SummaryState -> Message' -> UpdateM (SummaryState, [Command'])
update s (AppMsg (EndpointChange (Connected _ schema))) = do
  let statistics = computeStatistics schema
  cont s {_susStatistics = Just statistics}
update s _ = cont s
