module MDTypes
(MarkDownType(..))
where

import qualified Data.Tree as T

data MarkDownType =
    Header Level String
  | Lists (T.Tree String)
  | Highlight Color String
  | Text Font Color String
  | SplitLine

type Level = String
type Color = String -- For now
type Font = String -- For now
