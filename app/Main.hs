module Main (main) where

import qualified Parser.MDParser as P
import Control.Monad
import Visualizer.MDVisualizer as V
import MDTypes as MDT
import Data.List.Split
import Visualizer.BigHeader
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let slides = splitOn "===" content 
  let linesOfFiles = fmap P.parseSlide slides
  -- print linesOfFiles
  V.startTMD linesOfFiles
  return ()
