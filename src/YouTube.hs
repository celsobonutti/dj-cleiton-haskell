{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module YouTube where

import           Data.Aeson
import           Data.Text
import           Discord.Internal.Types.Embed
import           Network.HTTP.Req
import           Protolude               hiding ( Option )
import qualified Queue

default (Text)

fetch token query = req
    GET
    (https "youtube.googleapis.com" /: "youtube" /: "v3" /: "search")
    NoReqBody
    jsonResponse
    (options token query)

options token query =
    ("key" =: token)
        <> ("part" =: "snippet")
        <> ("q" =: query)
        <> ("maxResults" =: "1")

newtype VideoId = VideoId { videoId :: Text } deriving (Generic, Show)

instance ToJSON VideoId where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON VideoId

data VideoThumbnail = VideoThumbnail
    { url    :: Text
    , width  :: Int
    , height :: Int
    }
    deriving (Generic, Show)

instance ToJSON VideoThumbnail where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON VideoThumbnail

newtype VideoThumbnails = VideoThumbnails
    { medium :: VideoThumbnail
    } deriving (Generic, Show)

instance ToJSON VideoThumbnails where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON VideoThumbnails

data VideoSnippet = VideoSnippet
    { title       :: Text
    , description :: Text
    , thumbnails  :: VideoThumbnails
    }
    deriving (Generic, Show)

instance ToJSON VideoSnippet where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON VideoSnippet

data Video = Video
    { id      :: VideoId
    , snippet :: VideoSnippet
    }
    deriving (Generic, Show)

instance ToJSON Video where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Video

newtype Response = Response
  { items :: [Video]
  } deriving (Generic, Show)

instance ToJSON Response where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Response

toSong :: Response -> Maybe Queue.Song
toSong Response { items = [] }         = Nothing
toSong Response { items = (song : _) } = Just
    (Queue.Song { title       = title . snippet $ song
                , description = description . snippet $ song
                , videoId     = videoId . id $ song
                , thumbnail   = url . medium . thumbnails . snippet $ song
                }
    )
