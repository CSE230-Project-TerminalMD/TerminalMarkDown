module Visualizer.MDVisualizer
(startTMD)
where

import qualified MDTypes as MDT
import Visualizer.ElementVisualizer
import qualified Brick.Widgets.Center as C
import Brick.Main as M
import Brick

-- Naive Implementation for App State
data MDAppState = MDAppState {
  mds :: [MDT.MarkDownType],
  layout :: String,
  anime :: String,
  exit :: Bool
}

getAppState :: [MDT.MarkDownType] -> IO (MDAppState)
getAppState mds = return $ MDAppState mds "" "" True

visualizeMD :: MDAppState -> [Widget ()]
visualizeMD (MDAppState mds l a _) = [ui]
  where
    ui = C.center $ vBox $ map visualizeElement mds

-- Main App for terminal markdown
tmdApp :: M.App MDAppState e ()
tmdApp = M.App {
  M.appDraw = visualizeMD,
  M.appHandleEvent = M.resizeOrQuit,
  M.appStartEvent = return (),
  M.appAttrMap = const elementAttr,
  M.appChooseCursor = M.neverShowCursor
}

startTMD :: [MDT.MarkDownType] -> IO MDAppState
startTMD mds = do
    cur <- getAppState mds
    M.defaultMain tmdApp cur
