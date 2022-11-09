import Control.Monad
import Data.Char

-- Usage
-- $ ghc --make readFile.hs 
-- $ ./readFile < README.md

myfunc ('-':' ': restOfString) = "\ESC[0m" ++ "⚫︎" ++ restOfString
myfunc ('#':' ': restOfString) =  "\ESC[31m" ++ restOfString
myfunc ('#':'#':' ': restOfString) =  "\ESC[33m" ++ restOfString
myfunc s = "\ESC[0m" ++ s

main = forever $ do
  l <- getLine
  putStrLn $ myfunc l