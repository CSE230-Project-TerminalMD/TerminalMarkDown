{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import Parser.MDParser
import MDTypes

main :: IO ()
main = runTests
    [
        parserTests
    ]

parserTests :: Score -> TestTree
parserTests sc = testGroup "Parser Tests"
    [
        scoreTest ((\_ -> parseNoEither "Simple Plain"), (), [Plain "Simple Plain"], 2, "plain text")
    ]
    where
        scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
        scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
