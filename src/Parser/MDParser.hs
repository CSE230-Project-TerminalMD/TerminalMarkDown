module Parser.MDParser
(parseMD,
parseSlide)
where

import qualified MDTypes as MDT

import Text.Parsec hiding (State, between)
import Text.Parsec.String
import Text.Parsec.Char 

-- Text style parsers

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

parseStyleStrikeThrough :: Parser [MDT.TextStyleType]
parseStyleStrikeThrough = do
                            s1 <- manyTill anyChar (try (string "~~"))
                            text <- manyTill anyChar (try (string "~~"))
                            s2 <- many anyChar
                            return ((parseNoEither s1) ++ [MDT.Strikethrough text] ++ (parseNoEither s2))

parseStyleUnderline :: Parser [MDT.TextStyleType]
parseStyleUnderline = do
                            s1 <- manyTill anyChar (try (string "<u>"))
                            text <- manyTill anyChar (try (string "</u>"))
                            s2 <- many anyChar
                            return ((parseNoEither s1) ++ [MDT.Underline text] ++ (parseNoEither s2))

parseStyleInlineCode :: Parser [MDT.TextStyleType]
parseStyleInlineCode = do
                            s1 <- manyTill anyChar (try (string "`"))
                            text <- manyTill anyChar (try (string "`"))
                            s2 <- many anyChar
                            return ((parseNoEither s1) ++ [MDT.InlineCode text] ++ (parseNoEither s2))

parseStyle :: Parser [MDT.TextStyleType]
parseStyle = try (parseStyleBold) <|> try (parseStyleItalic) <|> try (parseStyleStrikeThrough) <|> try (parseStyleUnderline) <|> try (parseStyleInlineCode) <|> parseStylePlain

-- Return empty string if parsing fails
parseNoEither :: String -> [MDT.TextStyleType]
parseNoEither text = either (const [MDT.Error "Error Message"]) id (parse parseStyle "" text)

-- Top level markdown parsers
parseMkd :: String -> MDT.MarkDownType
parseMkd ('#':' ': text) = MDT.Header "1" (parseNoEither text)
parseMkd ('#':'#':' ': text) = MDT.Header "2" (parseNoEither text)
parseMkd ('#':'#':'#':' ': text) = MDT.Header "3" (parseNoEither text)
parseMkd ('-':' ': text) = MDT.ListBullet "1" (parseNoEither text)
parseMkd ('\t':'-':' ': text) = MDT.ListBullet "2" (parseNoEither text)
parseMkd ('>':' ': text) = MDT.Quote (parseNoEither text)
parseMkd text = (MDT.PlainText (parseNoEither text))

-- String in one slide -> List of parsed MarkDownTypes.
parseSlide :: String -> [MDT.MarkDownType]
parseSlide context = do
                        let linesOfSlide = lines context
                        let mkdOfSlide = fmap parseMkd linesOfSlide -- fix this to MarkdownType
                        mkdOfSlide

parseMD :: String -> (MDT.MarkDownType)
parseMD s = error "Developing"
