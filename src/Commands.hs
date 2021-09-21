module Commands where

import           Data.Text
import           Protolude

data T
  = Play Text
  | Remove Int
  | Skip
  | List
  deriving (Eq, Ord, Show)
