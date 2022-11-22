module MDTypes
(MarkDownType(..))
where

data MarkDownType =
    Header Level String
  | Lists [String]
  | Highlight Color String
  | Text Font Color String
  | SplitLine

type Level = String
type Color = String -- For now
type Font = String -- For now
