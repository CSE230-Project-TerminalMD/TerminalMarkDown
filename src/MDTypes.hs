module MDTypes where

-- data MarkDownType =
--     Header Level String
--   | Lists [String]
--   | Highlight Color String
--   | Text Font Color String
--   | SplitLine

data TextStyleType = 
    Plain String
  | Bold String
  | Italic String
  | Strikethrough String
  | Underline String
  | InlineCode String
  | Error String
  deriving (Show)

data MarkDownType =
    Header Level [TextStyleType]
  | ListBullet Level [TextStyleType]
  | PlainText [TextStyleType]
  | Quote [TextStyleType]
  deriving (Show)

type Level = String
type Color = String -- For now
type Font = String -- For now

type Slide = [MarkDownType]
type Slides = [Slide]
