module Main (main) where

import qualified Parser.MDParser as P
import Control.Monad
import Visualizer.MDVisualizer as V
import MDTypes as MDT

main :: IO ()
main = do
  V.startTMD [Header "" "SimpleDemo",
              Text "" "" "Hello world",
              Highlight "" "Quan",
              Lists ["I list something", "I list another"]]
  return ()
