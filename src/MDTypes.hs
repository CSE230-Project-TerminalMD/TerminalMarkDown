module MDTypes 
(TextStyleType(..),
MarkDownType(..),
Slide,
Slides,
SlideBlock)
where

data TextStyleType = 
    Plain String
  | Bold String
  | Italic String
  | Strikethrough String
  | Underline String
  | InlineCode String
  | Error String
  deriving (Eq, Show)

-- Should be able to marge to above
data MarkDownType =
    Header Level [TextStyleType]
  | ListBullet Level [TextStyleType]
  | PlainText [TextStyleType]
  | Quote [TextStyleType]
  | BigHeader String
  | SlideImage String
  | ErrorBlock String
  deriving (Show)

type Level = Int

type SlideBlock = [MarkDownType]
type Slide = [SlideBlock]
type Slides = [Slide]
