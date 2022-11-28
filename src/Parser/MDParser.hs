module Parser.MDParser
(parseMD,
naiveParse,
parseSlide)
where

import qualified MDTypes as MDT

import Text.Parsec hiding (State, between)
import Text.Parsec.String
import Text.Parsec.Char 

-- run :: MDT.MarkDownType -> IO ()
-- run stmt = do
--     putStrLn "Output Store:"
--     putStrLn (show (execS stmt empty))

-- heading1mk :: Parser MDT.Slide
-- heading1mk = do
--                 string "#"
--                 spaces -- TODO: at least one space
--                 s <- many1 anyChar
--                 return MDT.Slide [(MDT.Header "1" s)]

-- seqmk :: Parser MDT.Slide
-- seqmk = do
--             mkd <- heading1mk <|> heading1mk
--             string "\n"
--             mkd ++ fmap slideP

-- slideP :: Parser MDT.Slide
-- slideP = seqmk <|> heading1mk


-- slidesP :: Parser MDT.Slides
-- slidesP = do
--             -- s <- slideP
--             -- string "---"
--             -- s ++ (fmap slidesP)
--             MDT.Slides [[MDT.Header "" "Second Page"], [MDT.Header "" "Second Page"]]

-- parseFile :: FilePath -> IO (Either ParseError MDT.Slides)
-- parseFile f = parseFromFile slidesP f

-- runFile :: FilePath -> IO ()
-- runFile s = do
--   p <- parseFile s
--   case p of
--     Left err   -> print err
--     Right mkd -> V.startTMD mkd

-- do pattern matching, no need for parser, are you sure?
-- what 

-- parseStyle :: String -> [MDT.TextStyleType]
-- parseStyle (s1:'*':'*':text:'*':'*':s2) = (parseStyle s1) ++ (MDT.Bold text) ++ (parseStyle s2)
-- parseStyle s = MDT.Plain s

parseStylePlain :: Parser [MDT.TextStyleType]
parseStylePlain = do
                    text <- many anyChar
                    return [MDT.Plain text]

parseStyleBold :: Parser [MDT.TextStyleType]
parseStyleBold = do
                    s1 <- manyTill anyChar (try (string "**"))
                    text <- manyTill anyChar (try (string "**"))
                    s2 <- many anyChar
                    return ((parseNoEither s1) ++ [MDT.Bold text] ++ (parseNoEither s2))

parseStyleItalic :: Parser [MDT.TextStyleType]
parseStyleItalic = do
                    s1 <- manyTill anyChar (try (string "*"))
                    text <- manyTill anyChar (try (string "*"))
                    s2 <- many anyChar
                    return ((parseNoEither s1) ++ [MDT.Italic text] ++ (parseNoEither s2))

parseStyle :: Parser [MDT.TextStyleType]
parseStyle = try (parseStyleBold) <|> try (parseStyleItalic) <|> parseStylePlain

-- parseStyle :: Parser [MDT.TextStyleType]
-- parseStyle = do
--                 s1 <- manyTill anyChar (try (string "**"))
--                 text <- manyTill anyChar (try (string "**"))
--                 return (parseStyle s1) ++ (MDT.Bold text) ++ (parseStyle s2)


parseNoEither :: String -> [MDT.TextStyleType]
parseNoEither text = either (const []) id (parse parseStyle "" text)


parseMkd :: String -> MDT.MarkDownType
parseMkd ('#':' ': text) = MDT.Header "1" (parseNoEither text)
parseMkd text = MDT.PlainText (parseNoEither text)

-- String in one slide -> List of parsed MarkDownTypes.
-- 1. String in one slide to a list of each line.
-- 2. 
parseSlide :: String -> [MDT.MarkDownType]
parseSlide context = do
                        let linesOfSlide = lines context
                        let mkdOfSlide = fmap parseMkd linesOfSlide -- fix this to MarkdownType
                        mkdOfSlide

-- Bold, Italic, underline, strikeline, Inline Code, link can be in any text.
-- Top level: headers, lists, quote, plain text (deal with nesting in lists later)
-- set "\ESC[0m" as color and style
-- consider when to use "\ESC[0m" to cancel any style (top level?)


naiveParse :: String -> String
naiveParse ('-':' ': restOfString) = "\ESC[0m" ++ "⚫︎" ++ restOfString
naiveParse ('#':' ': restOfString) =  "\ESC[31m" ++ restOfString
naiveParse ('#':'#':' ': restOfString) =  "\ESC[33m" ++ restOfString
naiveParse s = "\ESC[0m" ++ s

parseMD :: String -> (MDT.MarkDownType)
parseMD s = error "Developing"
