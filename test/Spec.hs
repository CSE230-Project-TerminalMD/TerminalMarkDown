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
        scoreTest ((\_ -> parseNoEither "Simple Plain"), (), [Plain "Simple Plain"], 1, "plain text"),
        scoreTest ((\_ -> parseNoEither "*italics text*"), (), [Italic "italics text"], 1, "italics text"),
        scoreTest ((\_ -> parseNoEither "**bold text**"), (), [Bold "bold text"], 1, "bold text"),
        scoreTest ((\_ -> parseNoEither "~~strike through~~"), (), [Strikethrough "strike through"], 1, "strike through text"),
        scoreTest ((\_ -> parseNoEither "<u>underline text</u>"), (), [Underline "underline text"], 1, "underline text"),
        scoreTest ((\_ -> parseNoEither "`inline code`"), (), [InlineCode "inline code"], 1, "inline code text")
    ]
    where
        scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
        scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
