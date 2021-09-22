module Rasta where

import           Data.Text
import           Protolude
import qualified Queue
import           System.Process.Typed

play :: Queue.Song -> IO ()
play Queue.Song { videoId } = do
    let youtubeDl = setStdout createPipe $ proc
            "youtube-dl"
            ["-f", "bestaudio", "-o", "-", "-q", unpack videoId]
    withProcessWait_ youtubeDl $ \yt -> do
        let stdout = useHandleOpen . getStdout $ yt

        let ffmpeg = setStdin stdout $ proc
                "ffmpeg"
                [ "-ac"
                , "2"
                , "-i"
                , "pipe:0"
                , "-ar"
                , "48000"
                , "-f"
                , "s16le"
                , "-acodec"
                , "libopus"
                , "-loglevel"
                , "quiet"
                , "pipe:1"
                ]
        runProcess_ ffmpeg
