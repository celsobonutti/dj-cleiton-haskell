module Queue where

import           Data.IORef
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Sequence                  ( (<|)
                                                , Seq(..)
                                                )
import qualified Data.Sequence                 as Seq
import           Data.Text               hiding ( intersperse
                                                , map
                                                )
import           Protolude               hiding ( map )

type Song = Text

newtype Queue = Queue {songs :: Seq Song}
  deriving (Show)

empty :: Queue
empty = Queue Seq.empty

map :: (Seq Song -> Seq Song) -> Queue -> Queue
map f = Queue . f . songs

addSong :: Song -> Queue -> Queue
addSong = map . (:<|)

skip :: Queue -> Queue
skip = map skip'
 where
  skip' Empty         = Empty
  skip' (songs :|> _) = songs

removeSongByIndex :: Int -> Queue -> Queue
removeSongByIndex = map . Seq.deleteAt

print :: Queue -> Text
print = unlines . toList . Seq.mapWithIndex print . songs
 where
  print :: Int -> Song -> Text
  print index song = show index <> ". " <> show song
