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
  , black, withURL, imageHeight
  )
import Data.Graph (components)
import qualified Data.Text as T
import qualified Brick.Widgets.Border.Style as BS
import Visualizer.BigHeader
import Brick.Widgets.Border.Style ( BorderStyle(..) )
import Brick.Widgets.Center (hCenter, vCenter)
import qualified Data.Map as Map
import Codec.Picture
import Data.Either (isLeft, fromLeft, fromRight, rights)
import Data.Maybe (fromJust, isNothing)
import Data.Word (Word8)

-- Style for border
quoteBorder :: BorderStyle
quoteBorder =
    BorderStyle { bsCornerTL = '┏'
                , bsCornerTR = ' '
                , bsCornerBR = '┛'
                , bsCornerBL = ' '
                , bsIntersectFull = ' '
                , bsIntersectL = ' '
                , bsIntersectR = ' '
                , bsIntersectT = ' '
                , bsIntersectB = ' '
                , bsHorizontal = ' '
                , bsVertical = ' '
                }

-- Style for elements
elementAttr :: AttrMap
elementAttr = attrMap defAttr $
    [ (attrName "Header1", withStyle (red `on` rgbColor 255 179 179) bold)
    , (attrName "Header2", withStyle (green `on` rgbColor 179 255 153) bold)
    , (attrName "Header3", withStyle ((rgbColor 255 153 0) `on` rgbColor 255 224 179) underline)
    , (attrName "Header4", blue `on` rgbColor 179 217 255)
    , (attrName "Italic", withStyle defAttr italic)
    , (attrName "snakeAttr", blue `on` blue)
    , (attrName "Bold", withStyle defAttr bold)
    , (attrName "Strikethrough", withStyle defAttr strikethrough)
    , (attrName "Underline", withStyle defAttr underline)
    , (attrName "InlineCode", (rgbColor 255 140 57) `on` rgbColor 34 37 41)
    , (attrName "Quote", rgbColor 30 30 30 `on` rgbColor 120 120 120)
    , (attrName "Default", defAttr)
    ] ++ composition1 ++ rgbmaps
    where
        rgbmaps = [(attrName (show (PixelRGB8 (int2Word8 r) (int2Word8 g) (int2Word8 b))), rgbColor r g b `on` rgbColor r g b) | r <- [255,254..230], g <- [255,254..150], b <- [255,254..150]]
        composition1 = [Data.Bifunctor.bimap (fst mdt <>) (withStyle (snd mdt)) style |
                                              mdt <- [(attrName "Default", defAttr)
                                                    , (attrName "Header1", withStyle (red `on` rgbColor 255 179 179) bold)  
                                                    , (attrName "Header2", withStyle (green `on` rgbColor 179 255 153) bold)
                                                    , (attrName "Header3", withStyle ((rgbColor 255 153 0) `on` rgbColor 255 224 179) underline)
                                                    , (attrName "Header4", blue `on` rgbColor 179 217 255)],
                                              style <- [(attrName "Italic", italic )
                                                      , (attrName "Bold", bold)
                                                      , (attrName "Underline", underline)
                                                      , (attrName "Strikethrough", strikethrough)]]
int2Word8 :: Int -> Word8
int2Word8 r = fromIntegral r

-- TextStyle Visualizer
visualizeTextStyle :: AttrName -> TextStyleType -> Widget ()
visualizeTextStyle a (Bold s) = withAttr (a <> attrName "Bold") (str s)
visualizeTextStyle a (Plain s) = withAttr a (str s)
visualizeTextStyle a (Italic s) = withAttr (a <> attrName "Italic") (str s)
visualizeTextStyle a (Underline s) = withAttr (a <> attrName "Underline") (str s)
visualizeTextStyle a (Strikethrough s) = withAttr (a <> attrName "Strikethrough") (str s)
visualizeTextStyle _ (InlineCode s) = forceAttr (attrName "InlineCode") (str s)
visualizeTextStyle _ _ = str "wait for implement"

-- Image Visualizer
visualizePixelLine :: Image PixelRGB8 -> Int -> Int -> Widget ()
visualizePixelLine img x y 
    | x < Codec.Picture.imageWidth img = hBox (visualizePixelLine img (x+1) y:[withAttr (attrName (show (pixelAt img x y))) (str "  ")])
    | otherwise = str ""

visualizeImg :: Either String DynamicImage -> Widget ()
visualizeImg img
    | isLeft img = str "❌ Image Not Found"
    | otherwise = vLimit (Codec.Picture.imageHeight rgbimg) $ vBox [visualizePixelLine rgbimg 0 y | y <- [0..(Codec.Picture.imageHeight rgbimg)]]
        where
            rgbimg = convertRGB8 $ head (rights [img])

-- Element Visualizer
visualizeElement :: Map.Map String (Either String DynamicImage) -> MarkDownType -> Widget ()
visualizeElement _ (BigHeader s) =
    vLimit 8 $
    vCenter $
    hBox $
    Prelude.map (str . fontChar) s
visualizeElement _ (Header level ts) =
    hBox $
    withAttr (attrName curHeader) (str prefix):Prelude.map (visualizeTextStyle (attrName curHeader)) ts
    where
        curHeader = "Header" ++ show level
        prefix = prefixes !! (level - 1)
        prefixes = ["🍉 ", "🐸 ", "🐤 ", "🧊 "]
visualizeElement _ (PlainText ts) = hBox $ Prelude.map (visualizeTextStyle (attrName "Default")) ts
visualizeElement _ (Quote ts) =
    withBorderStyle quoteBorder $
    border $
    hBox $
    [str " "] ++ Prelude.map (visualizeTextStyle (attrName "Quote")) ts ++ [str " "]
visualizeElement _ (ListBullet l ts) = hBox (str (spaces ++ special):Prelude.map (visualizeTextStyle (attrName "Default")) ts)
    where
        spaces = concat (replicate ((l-1)*4) " ")
        special | l `mod` 2 == 1 = "● "
                | otherwise = "○ "
visualizeElement imgs (SlideImage s)
    |isNothing (Map.lookup s imgs) = str "Nothing" -- Not gonna happen
    | otherwise = visualizeImg (fromJust (Map.lookup s imgs))
visualizeElement _ _ = str ""
