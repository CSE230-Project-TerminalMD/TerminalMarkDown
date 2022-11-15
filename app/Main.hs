module Main (main) where

import qualified Parser.MDParser as P
import Control.Monad

main :: IO ()
main = forever $ do
    l <- getLine
    putStrLn $ P.naiveParse l
