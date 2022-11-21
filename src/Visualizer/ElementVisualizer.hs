module Visualizer.ElementVisualizer
(visualizeElement)
where

import qualified MDTypes as MDT
import Brick

visualizeElement :: MDT.MarkDownType -> Widget ()
visualizeElement (MDT.Text f c s) = str s
