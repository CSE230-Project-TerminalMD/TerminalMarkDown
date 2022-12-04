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

-- Naive Implementation for App State
data MDAppState = MDAppState {
  mds :: MDT.Slides,
  cur :: Int,
  layout :: String,
  anime :: String
}

initialState :: MDT.Slides -> IO (MDAppState)
initialState mds = return $ MDAppState mds 0 "" ""

visualizeBlock :: MDT.SlideBlock -> Widget ()
visualizeBlock blk = blk_ui
  where
    blk_ui = vBox  (map (C.center . visualizeElement) blk)

visualizeMD :: MDAppState -> [Widget ()]
visualizeMD (MDAppState slides i l a)
  | length slides == 1 = [visualizeBlock (head (slides!!i))]
  | otherwise = [hBox $ map visualizeBlock (slides!!i)]

-- Main App for terminal markdown
handleSlideEvent :: MDAppState -> BrickEvent () e -> EventM () (Next MDAppState)
handleSlideEvent s@(MDAppState mds i l a) (VtyEvent (V.EvKey V.KDown []))
  | i+1<length mds = continue (MDAppState mds (i+1) l a)
  | otherwise = halt s
handleSlideEvent s@(MDAppState mds i l a) (VtyEvent (V.EvKey V.KUp []))
  | i-1>=0 = continue (MDAppState mds (i-1) l a)
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
startTMD mds = do
    cur <- initialState mds
    M.defaultMain tmdApp cur
