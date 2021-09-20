module Commands where

import           Data.Text
import           Protolude

data T
  = Play Text
  | Remove Int
  | Skip
  deriving (Eq, Ord, Show)
