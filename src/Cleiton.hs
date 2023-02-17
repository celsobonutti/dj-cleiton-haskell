{-# LANGUAGE ScopedTypeVariables #-}

module Cleiton (
    rasta,
) where

import Commands
import Control.Monad (
    forM_,
    void,
    when,
 )
import Control.Monad.Reader
import Data.Aeson
import Data.IORef
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Internal.Rest as R
import qualified Discord.Requests as R
import Discord.Types
import Network.HTTP.Req
import qualified Parser
import Queue (Queue)
import qualified Queue
import System.Environment (getEnv)
import Text.Megaparsec (parseMaybe)
import UnliftIO (liftIO)
import UnliftIO.Concurrent
import qualified YouTube
import Prelude hiding (readFile)

commandParser = Parser.make "λ"

rasta :: IO ()
rasta = do
    queue <- newIORef Queue.empty
    tok <- pack <$> getEnv "DISCORD_TOKEN"
    youtubeToken <- pack <$> getEnv "YOUTUBE_TOKEN"

    t <-
        runDiscord $
            def
                { discordToken = tok
                , discordOnStart = startHandler
                , discordOnEnd = liftIO $ TIO.putStrLn "Ended"
                , discordOnEvent = eventHandler queue youtubeToken
                , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                }
    TIO.putStrLn t

startHandler :: DiscordHandler ()
startHandler = do
    Right partialGuilds <- restCall R.GetCurrentUserGuilds

    let activity =
            def
                { activityName = "cleiton-rasta"
                , activityType = ActivityTypeGame
                , activityUrl = Nothing
                }
    let opts =
            UpdateStatusOpts
                { updateStatusOptsSince = Nothing
                , updateStatusOptsGame = Just activity
                , updateStatusOptsNewStatus = UpdateStatusOnline
                , updateStatusOptsAFK = False
                }
    sendCommand (UpdateStatus opts)

eventHandler :: IORef Queue -> Text -> Event -> DiscordHandler ()
eventHandler queue youtubeToken event = case event of
    MessageCreate message -> do
        let channel = message.messageChannelId
            sendMessage content = void $ restCall (R.CreateMessage channel content)
        case parseMaybe commandParser (message.messageContent) of
            Just (Play song) -> addSong queue youtubeToken channel song
            Just List -> do
                queue <- liftIO . readIORef $ queue
                sendMessage $ Queue.print queue
            Just Skip -> do
                result <- liftIO $ atomicModifyIORef' queue Queue.skip
                case result of
                    Just removed -> sendMessage $ "Tirando: " <> Queue.title removed
                    Nothing -> sendMessage "Tem música não porra"
            _ -> liftIO $ TIO.putStrLn message.messageContent
    _ -> return ()

addSong ::
    IORef Queue -> Text -> ChannelId -> Text -> ReaderT DiscordHandle IO ()
addSong queue youtubeToken channel song = do
    request <- liftIO . runReq defaultHttpConfig $ YouTube.fetch youtubeToken song
    case YouTube.toSong . responseBody $ request of
        Just song -> do
            liftIO $ modifyIORef' queue (Queue.addSong song)
            newQueue <- liftIO . readIORef $ queue
            void $
                restCall
                    ( R.CreateMessageDetailed
                        channel
                        ( def
                            { R.messageDetailedContent = "Bora debochar legal com"
                            , R.messageDetailedEmbeds = Just [Queue.toEmbed song]
                            }
                        )
                    )
        Nothing ->
            void $
                restCall
                    (R.CreateMessage channel "Deu ruim, essa porra existe mesmo?")
