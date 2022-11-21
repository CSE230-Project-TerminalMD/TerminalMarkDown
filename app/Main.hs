module Main (main) where

import qualified Parser.MDParser as P
import Control.Monad
import Visualizer.MDVisualizer as V
import MDTypes as MDT

main :: IO ()
main = do
  V.startTMD [Text "" "" "Hello world"]
  return ()
