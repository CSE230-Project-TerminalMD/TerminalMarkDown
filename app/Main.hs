module Main (main) where

import qualified Parser.MDParser as P
import Control.Monad
import Visualizer.MDVisualizer as V
import MDTypes as MDT
import Data.List.Split
import Visualizer.BigHeader
import System.Environment (getArgs)
import Data.Word (Word8)
import Codec.Picture

sometest :: Int -> Word8
sometest r = fromIntegral r

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let slides = splitOn "===" content 
  let linesOfFiles = fmap P.parseSlide slides
  print linesOfFiles
  -- print (show (PixelRGB8 (sometest 255) (sometest 254) (sometest 109)))
  V.startTMD linesOfFiles
  return ()
