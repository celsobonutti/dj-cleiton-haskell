{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cleiton
  ( rasta
  ) where

import           Commands
import           Control.Monad                  ( forM_
                                                , void
                                                , when
                                                )
import           Data.Aeson
import           Data.IORef
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Discord
import qualified Discord.Requests              as R
import           Discord.Types
import           Network.HTTP.Req
import qualified Parser
import           Protolude               hiding ( String
                                                , putStrLn
                                                , threadDelay
                                                )
import           Queue                          ( Queue )
import qualified Queue
import           Text.Megaparsec                ( parseMaybe )
import           UnliftIO                       ( liftIO )
import           UnliftIO.Concurrent
import qualified YouTube

data Video = Video
  { title :: Text
  , id    :: Text
  }
  deriving (Generic, Show)

instance ToJSON Video where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Video

commandParser = Parser.make "λ"

rasta :: IO ()
rasta = do
  queue        <- newIORef Queue.empty
  tok          <- TIO.readFile "./auth-token.secret"
  youtubeToken <- readFile "./auth-token.secret"

  t <- runDiscord $ def { discordToken   = tok
                        , discordOnStart = startHandler
                        , discordOnEnd   = liftIO $ TIO.putStrLn "Ended"
                        , discordOnEvent = eventHandler queue youtubeToken
                        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                        }
  TIO.putStrLn t

startHandler :: DiscordHandler ()
startHandler = do
  Right partialGuilds <- restCall R.GetCurrentUserGuilds

  let activity = Activity { activityName = "cleiton-rasta"
                          , activityType = ActivityTypeGame
                          , activityUrl  = Nothing
                          }
  let opts = UpdateStatusOpts { updateStatusOptsSince     = Nothing
                              , updateStatusOptsGame      = Just activity
                              , updateStatusOptsNewStatus = UpdateStatusOnline
                              , updateStatusOptsAFK       = False
                              }
  sendCommand (UpdateStatus opts)

eventHandler :: IORef Queue -> Text -> Event -> DiscordHandler ()
eventHandler queue youtubeToken event = case event of
  MessageCreate m -> do
    case parseMaybe commandParser (messageText m) of
      Just (Play song) -> do
        void
          $ restCall
              (R.CreateMessage (messageChannel m)
                               ("taca-lhe pau playboy: " <> song)
              )
        liftIO $ modifyIORef' queue $ Queue.addSong song
        -- request <- liftIO . runReq defaultHttpConfig $ YouTube.fetch
        --   youtubeToken
        --   song
        -- liftIO $ print (responseBody r :: Video)
        newQueue <- liftIO . readIORef $ queue
        void $ restCall
          (R.CreateMessage (messageChannel m) (Queue.print newQueue))
      Just (Remove number) -> do
        void $ restCall (R.CreateMessage (messageChannel m) "ê rural")
      _ -> liftIO $ TIO.putStrLn (messageText m)
  _ -> return ()

isTextChannel :: Channel -> Bool
isTextChannel ChannelText{} = True
isTextChannel _             = False
