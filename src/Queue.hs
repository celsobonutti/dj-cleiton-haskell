module Queue where

import Data.Default
import Data.Foldable (toList)
import Data.IORef
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text hiding (
    intersperse,
    map,
 )
import Discord.Internal.Types.Embed
import Prelude hiding (map, unlines)

data Song = Song
    { title :: Text
    , videoId :: Text
    , description :: Text
    , thumbnail :: Text
    }
    deriving (Eq)

newtype Queue = Queue {songs :: Seq Song}

empty :: Queue
empty = Queue Seq.empty

map :: (Seq Song -> Seq Song) -> Queue -> Queue
map f = Queue . f . songs

addSong :: Song -> Queue -> Queue
addSong song = map (:|> song)

skip :: Queue -> (Queue, Maybe Song)
skip Queue{songs = Empty} = (Queue Empty, Nothing)
skip Queue{songs = (song :<| songs)} = (Queue songs, Just song)

removeSongByIndex :: Int -> Queue -> Queue
removeSongByIndex = map . Seq.deleteAt

print :: Queue -> Text
print = unlines . toList . Seq.mapWithIndex print . songs
  where
    print :: Int -> Song -> Text
    print index song = (pack . show . (+ 1) $ index) <> ". " <> title song

toEmbed :: Song -> CreateEmbed
toEmbed song =
    def
        { createEmbedTitle = title song
        , createEmbedDescription = description song
        , createEmbedUrl = "https://www.youtube.com/watch?v=" <> videoId song
        , createEmbedThumbnail = Just (CreateEmbedImageUrl . thumbnail $ song)
        }
