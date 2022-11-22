module Visualizer.ElementVisualizer
(visualizeElement,
elementAttr)
where

import qualified MDTypes as MDT
import Brick
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty
  ( Attr, white, blue, cyan, green, red, yellow
  , black, withURL
  )

elementAttr :: AttrMap
elementAttr = attrMap defAttr
    [ (attrName "normal_text", white `on` black)
    , (attrName "Header1", white `on` blue)
    , (attrName "highlight", black `on` white)
    ]

toText :: String -> String -> MDT.MarkDownType
toText prefix s = MDT.Text "" "" $ prefix ++ s

visualizeElement :: MDT.MarkDownType -> Widget ()
visualizeElement (MDT.Text f c s) = str s

visualizeElement (MDT.Header l s) = withAttr (attrName $ "Header1") $ str s

visualizeElement (MDT.Highlight c s) = withAttr (attrName $ "highlight") $ str s

visualizeElement (MDT.SplitLine) = str ""

visualizeElement (MDT.Lists []) = str ""
visualizeElement (MDT.Lists s) = vBox $ map (visualizeElement . toText "⚫︎") s