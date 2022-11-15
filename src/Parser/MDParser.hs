module Parser.MDParser
(parseMD,
naiveParse)
where

import qualified MDTypes as MDT

naiveParse :: String -> String
naiveParse ('-':' ': restOfString) = "\ESC[0m" ++ "⚫︎" ++ restOfString
naiveParse ('#':' ': restOfString) =  "\ESC[31m" ++ restOfString
naiveParse ('#':'#':' ': restOfString) =  "\ESC[33m" ++ restOfString
naiveParse s = "\ESC[0m" ++ s

parseMD :: String -> (MDT.MarkDownType)
parseMD s = error "Developing"
