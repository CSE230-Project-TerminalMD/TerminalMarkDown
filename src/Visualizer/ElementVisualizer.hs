module Visualizer.ElementVisualizer
(visualizeElement,
elementAttr)
where

import MDTypes
import Brick
import Graphics.Vty.Attributes
import Data.Bifunctor
import Brick.Widgets.Border
import Graphics.Vty
  ( Attr, white, blue, cyan, green, red, yellow
  , black, withURL
  )
import Data.Graph (components)
import qualified Data.Text as T
import qualified Brick.Widgets.Border.Style as BS
import Visualizer.BigHeader

borderStyles :: [(T.Text, BS.BorderStyle)]
borderStyles =
    [ (T.pack "ascii", BS.ascii)
    , (T.pack "unicode", BS.unicode)
    , (T.pack "unicode bold", BS.unicodeBold)
    , (T.pack "unicode rounded", BS.unicodeRounded)
    , (T.pack "from 'x'", BS.borderStyleFromChar 'x')
    ]

elementAttr :: AttrMap
elementAttr = attrMap defAttr $
    [ (attrName "Header1", withStyle (red `on` rgbColor 255 179 179) bold)
    , (attrName "Header2", withStyle (green `on` rgbColor 179 255 153) bold)
    , (attrName "Header3", withStyle ((rgbColor 255 153 0) `on` rgbColor 255 224 179) underline)
    , (attrName "Header4", blue `on` rgbColor 179 217 255)
    , (attrName "Italic", withStyle defAttr italic)
    , (attrName "Bold", withStyle defAttr bold)
    , (attrName "Strikethrough", withStyle defAttr strikethrough)
    , (attrName "Underline", withStyle defAttr underline)
    , (attrName "InlineCode", (rgbColor 255 140 57) `on` rgbColor 34 37 41)
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

-- TextStyle Visualizer
visualizeTextStyle :: AttrName -> TextStyleType -> Widget ()
visualizeTextStyle a (Bold s) = withAttr (a <> attrName "Bold") (str s)
visualizeTextStyle a (Plain s) = withAttr a (str s)
visualizeTextStyle a (Italic s) = withAttr (a <> attrName "Italic") (str s)
visualizeTextStyle a (Underline s) = withAttr (a <> attrName "Underline") (str s)
visualizeTextStyle a (Strikethrough s) = withAttr (a <> attrName "Strikethrough") (str s)
visualizeTextStyle _ (InlineCode s) = forceAttr (attrName "InlineCode") (str s)
visualizeTextStyle _ _ = str "wait for implement"

-- Element Visualizer
visualizeElement :: MarkDownType -> Widget ()
visualizeElement (BigHeader s) = hBox $ map (str . fontChar) s
visualizeElement (Header level ts) = hBox $ withAttr (attrName curHeader) (str prefix):map (visualizeTextStyle (attrName curHeader)) ts
    where
        curHeader = "Header" ++ show level
        prefix = prefixes !! (level - 1)
        prefixes = ["🍓 ", "🥝 ", "🐤 ", "🦋 "]
visualizeElement (PlainText ts) = hBox $ map (visualizeTextStyle (attrName "Default")) ts
visualizeElement (Quote ts) = hBox $ map (visualizeTextStyle (attrName "Quote")) ts
visualizeElement (ListBullet l ts) = hBox (str (spaces ++ special):map (visualizeTextStyle (attrName "Default")) ts)
    where
        spaces = concat (replicate ((l-1)*4) " ")
        special | l `mod` 2 == 1 = "● "
                | otherwise = "○ "
       
