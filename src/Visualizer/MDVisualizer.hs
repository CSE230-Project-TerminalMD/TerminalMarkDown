module Visualizer.MDVisualizer
(startTMD)
where

import qualified MDTypes as MDT
import Visualizer.ElementVisualizer
import qualified Brick.Widgets.Center as C
import Brick.Main as M
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import Brick
import qualified Data.Map as Map
import Codec.Picture
import Graphics.Vty (Vty(nextEvent))

-- Naive Implementation for App State
data MDAppState = MDAppState {
  mds :: MDT.Slides,
  cur :: Int,
  imgs :: Map.Map String (Either String DynamicImage),
  layout :: String,
  anime :: String
}

getImgsFromBlock :: MDT.SlideBlock -> Map.Map String (Either String DynamicImage) -> IO (Map.Map String (Either String DynamicImage))
getImgsFromBlock ((MDT.SlideImage s):mdts) curMap =
  do
    curImg <- readImage s
    leftImgs <- getImgsFromBlock mdts curMap
    return $ Map.union (Map.insert s curImg curMap) leftImgs
getImgsFromBlock (_:mdts) curMap = getImgsFromBlock mdts curMap
getImgsFromBlock [] curMap = return curMap

getImgsFromSlide :: MDT.Slide -> Map.Map String (Either String DynamicImage) -> IO (Map.Map String (Either String DynamicImage))
getImgsFromSlide (b:bs) curMap =
  do
    slideMap <- getImgsFromBlock b curMap
    leftMap <- getImgsFromSlide bs curMap
    return $ Map.union slideMap leftMap
getImgsFromSlide [] curMap = return curMap

getImgsFromSlides :: MDT.Slides -> Map.Map String (Either String DynamicImage) -> IO (Map.Map String (Either String DynamicImage))
getImgsFromSlides (b:bs) curMap =
  do
    slideMap <- getImgsFromSlide b curMap
    leftMap <- getImgsFromSlides bs curMap
    return $ Map.union slideMap leftMap
getImgsFromSlides [] curMap = return curMap

initialState :: MDT.Slides -> IO MDAppState
initialState markdowns =
  do
    imgMap <- getImgsFromSlides markdowns Map.empty
    print (Map.keys imgMap)
    return $ MDAppState markdowns 0 imgMap "" ""

visualizeBlock :: Map.Map String (Either String DynamicImage) -> MDT.SlideBlock -> Widget ()
visualizeBlock images ((MDT.PlainText ts) : xs) =
  vBox (C.hCenterLayer (visualizeElement images (MDT.PlainText ts)) : [visualizeBlock images xs])

visualizeBlock images ((MDT.ListBullet l ts) : xs) =
  vBox (C.hCenterLayer (visualizeElement images (MDT.ListBullet l ts)) : [visualizeBlock images xs])

visualizeBlock images (x: (MDT.PlainText ts2) : xs) =
  C.hCenter . vBox $ visualizeBlock images [x] : visualizeBlock images [MDT.PlainText ts2] : [visualizeBlock images xs]

visualizeBlock images blk = blk_ui
  where
    blk_ui = vBox  (map (C.hCenter . visualizeElement images) blk)

visualizeMD :: MDAppState -> [Widget ()]
visualizeMD (MDAppState slides i images l a)
  | length slides == 1 = [C.center $ visualizeBlock images (head (slides!!i))]
  | otherwise = [C.center $ visualizeIfBigHeader (slides!!i)]
    where
      visualizeIfBigHeader ([MDT.BigHeader s]:bs) =
        vBox
        [vLimit 20 (C.center (visualizeBlock images [MDT.BigHeader s])),
        visualizeIfBigHeader bs]
      visualizeIfBigHeader slide = hBox $ map (visualizeBlock images) slide

-- Main App for terminal markdown
handleSlideEvent :: MDAppState -> BrickEvent () e -> EventM () (Next MDAppState)
handleSlideEvent s@(MDAppState markdowns i images l a) (VtyEvent (V.EvKey V.KDown []))
  | i+1<length markdowns = continue (MDAppState markdowns (i+1) images l a)
  | otherwise = halt s
handleSlideEvent s@(MDAppState markdowns i images l a) (VtyEvent (V.EvKey V.KUp []))
  | i-1>=0 = continue (MDAppState markdowns (i-1) images l a)
  | otherwise = halt s
handleSlideEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleSlideEvent s _ = continue s

tmdApp :: M.App MDAppState e ()
tmdApp = M.App {
  M.appDraw = visualizeMD,
  M.appHandleEvent = handleSlideEvent,
  M.appStartEvent = return,
  M.appAttrMap = const elementAttr,
  M.appChooseCursor = M.neverShowCursor
}

startTMD :: MDT.Slides -> IO MDAppState
startTMD markdowns = do
    curr <- initialState markdowns
    M.defaultMain tmdApp curr
