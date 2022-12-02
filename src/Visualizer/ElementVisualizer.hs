module Visualizer.ElementVisualizer
(visualizeElement,
elementAttr)
where

import MDTypes
import Brick
import Graphics.Vty.Attributes
import Data.Bifunctor
import Graphics.Vty
  ( Attr, white, blue, cyan, green, red, yellow
  , black, withURL
  )
import Data.Graph (components)

elementAttr :: AttrMap
elementAttr = attrMap defAttr $
    [ (attrName "Header1", fg red)
    , (attrName "Header2", fg yellow)
    , (attrName "Header3", fg green)
    , (attrName "Header4", withStyle (fg blue) underline)
    , (attrName "Italic", withStyle defAttr italic)
    , (attrName "Bold", withStyle defAttr bold)
    , (attrName "Strikethrough", withStyle defAttr strikethrough)
    , (attrName "Underline", withStyle defAttr underline)
    , (attrName "InlineCode", yellow `on` rgbColor 80 80 80)
    , (attrName "Quote", cyan `on` rgbColor 120 120 120)
    , (attrName "Default", defAttr)
    ] ++ composition1
    where
        composition1 = [Data.Bifunctor.bimap (fst mdt <>) (withStyle (snd mdt)) style |
                                              mdt <- [(attrName "Default", defAttr)
                                                    , (attrName "Header1", fg red)   
                                                    , (attrName "Header2", fg yellow)
                                                    , (attrName "Header3", fg green)
                                                    , (attrName "Header4", withStyle (fg blue) underline)],
                                              style <- [(attrName "Italic", italic )
                                                      , (attrName "Bold", bold)
                                                      , (attrName "Underline", underline)
                                                      , (attrName "Strikethrough", strikethrough)]]

visualizeTextStyle :: AttrName -> TextStyleType -> Widget ()
visualizeTextStyle a (Bold s) = withAttr (a <> attrName "Bold") (str s)
visualizeTextStyle a (Plain s) = withAttr a (str s)
visualizeTextStyle a (Italic s) = withAttr (a <> attrName "Italic") (str s)
visualizeTextStyle a (Underline s) = withAttr (a <> attrName "Underline") (str s)
visualizeTextStyle _ _ = str "wait for implement"

visualizeElement :: MarkDownType -> Widget ()
visualizeElement (Header level ts) = hBox $ map (visualizeTextStyle (attrName curHeader)) ts
    where
        curHeader = "Header" ++ level
visualizeElement (PlainText ts) = hBox $ map (visualizeTextStyle (attrName "Default")) ts
visualizeElement (Quote ts) = hBox $ map (visualizeTextStyle (attrName "Quote")) ts
visualizeElement (ListBullet l ts) = hBox ((str $ spaces ++ "⚫︎ "):(map (visualizeTextStyle (attrName "Default")) ts))
    where
        spaces = concat (replicate (((read l :: Int)-1)*4) " ")
