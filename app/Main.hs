module Main (main) where

import qualified Parser.MDParser as P
import Control.Monad
import Lib

main :: IO ()
main = forever $ do
    l <- getLine
    putStrLn $ P.naiveParse l
