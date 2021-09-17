module Queue where

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Protolude
import Data.Text hiding (intersperse)

data QueueState = QueueState { next :: Int
                             , songs :: Map Int Text
                             }

type Queue = IORef QueueState

empty :: IO Queue
empty = newIORef $ QueueState 0 Map.empty

addSong :: Queue -> Text -> IO ()
addSong queue song = modifyIORef' queue addSong'
  where
    addSong' QueueState { next, songs } = QueueState { next = next + 1
                                                     , songs = Map.insert next song songs
                                                     }

print :: QueueState -> Text
print = unlines . fmap printTuple . Map.toList . songs
  where
    printTuple :: (Int, Text) -> Text
    printTuple (fst, snd) = show fst <> ". " <> show snd
