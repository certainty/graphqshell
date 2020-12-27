{-# LANGUAGE RankNTypes #-}

-- | Assorted utilities that are shared between components
module Shell.Components.Utilities where

import           Brick                                                                  ( EventM
                                                                                        , Next
                                                                                        )
import           Brick.Widgets.Border                                                   ( )
import           Brick.Widgets.Border.Style                                             ( )
import           Lens.Micro.Platform                                                    ( Lens'
                                                                                        , set
                                                                                        , view
                                                                                        )
import           Relude                                                                 ( Applicative
                                                                                          ( pure
                                                                                          )
                                                                                        , ($)
                                                                                        , (<$>)
                                                                                        )
import           Shell.Components.Types                                                 ( )

-- Run update function for a component

-- | TODO: make sure Next actions are handled correctly (add unit tests)
updateComponent
  :: a -> Lens' a b -> (b -> e -> EventM n (Next b)) -> e -> EventM n (Next a)
updateComponent state target componentUpdate event = do
  newVal <- componentUpdate (view target state) event
  pure $ (\newState -> set target newState state) <$> newVal
