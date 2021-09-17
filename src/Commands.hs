module Commands where

import Data.Text
import Protolude

data T
  = Play Text
  | Remove Int
  deriving (Eq, Ord, Show)
