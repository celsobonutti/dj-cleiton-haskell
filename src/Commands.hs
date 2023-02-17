module Commands where

import Data.Text

data T
    = Play Text
    | Remove Int
    | Skip
    | List
    deriving (Eq, Ord, Show)
