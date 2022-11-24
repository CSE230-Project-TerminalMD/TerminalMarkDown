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
  anime :: String,
  exit :: Bool
}

initialState :: MDT.Slides -> IO (MDAppState)
initialState mds = return $ MDAppState mds 0 "" "" True

visualizeMD :: MDAppState -> [Widget ()]
visualizeMD (MDAppState mds i l a _) = [ui]
  where
    ui = C.center $ vBox $ map visualizeElement (mds!!i)

-- Main App for terminal markdown
handleSlideEvent :: MDAppState -> BrickEvent () e -> EventM () (Next MDAppState)
handleSlideEvent s@(MDAppState mds i l a e) (VtyEvent (V.EvKey V.KDown []))
  | e = continue s
  | otherwise = halt s
handleSlideEvent s _ = halt s

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
