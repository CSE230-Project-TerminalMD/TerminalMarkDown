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
  deriving (Eq, Show)

-- Should be able to marge to above
data MarkDownType =
    Header Level [TextStyleType]
  | ListBullet Level [TextStyleType]
  | ListBulletBlock [(Level, [TextStyleType])]
  | PlainText [TextStyleType]
  | Quote [TextStyleType]
  | BigHeader String
  | SlideImage String
  deriving (Show)

type Level = Int

type Block = [MarkDownType]
type SlideLater = [Block]
type Slide = [MarkDownType]
type Slides = [Slide]
