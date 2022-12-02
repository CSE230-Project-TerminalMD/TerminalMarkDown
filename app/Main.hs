module Main (main) where

import qualified Parser.MDParser as P
import Control.Monad
import Visualizer.MDVisualizer as V
import MDTypes as MDT
import Data.List.Split

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let slides = splitOn "---" content 
  let linesOfFiles = fmap P.parseSlide slides
  -- print linesOfFiles
  V.startTMD linesOfFiles
  -- slides <- P.parseFile (head fs)
  -- print slides
  -- case slides of
  --   Left err   -> print err
  --   Right mkd -> V.startTMD slides
  -- V.startTMD [[Header "" "SimpleDemo",
  --             Text "" "" "Hello world",
  --             Highlight "" "Quan",
  --             Lists ["I list something", "I list another"]],
  --             [Header "" "Second Page"]]
  return ()

