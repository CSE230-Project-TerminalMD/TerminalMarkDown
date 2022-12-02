module MDTypes 
(TextStyleType(..),
MarkDownType(..),
Slide,
Slides)
where

data TextStyleType = 
    Plain String
  | Bold String
  | Italic String
  | Strikethrough String
  | Underline String
  | InlineCode String
  | Error String
  deriving (Show)

-- Should be able to marge to above
data MarkDownType =
    Header Level [TextStyleType]
  | ListBullet Level [TextStyleType]
  | PlainText [TextStyleType]
  | Quote [TextStyleType]
  deriving (Show)

type Level = String

type Slide = [MarkDownType]
type Slides = [Slide]
