module Cleiton
    ( rasta
    ) where

import Control.Monad (when, forM_, void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO (liftIO)
import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Protolude hiding (threadDelay, String, ByteString, putStrLn)

import Text.Megaparsec (parseMaybe)
import qualified Parser
import Commands
import qualified Queue
import Data.IORef

commandParser = Parser.make "λ"

rasta :: IO ()
rasta = do
  queue <- Queue.empty
  tok <- TIO.readFile "./auth-token.secret"

  t <- runDiscord $ def { discordToken = tok
                        , discordOnStart = startHandler
                        , discordOnEnd = liftIO $ TIO.putStrLn "Ended"
                        , discordOnEvent = eventHandler queue
                        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                        }
  TIO.putStrLn t

startHandler :: DiscordHandler ()
startHandler = do
  Right partialGuilds <- restCall R.GetCurrentUserGuilds

  let activity = Activity { activityName = "cleiton-rasta"
                          , activityType = ActivityTypeGame
                          , activityUrl = Nothing
                          }
  let opts = UpdateStatusOpts { updateStatusOptsSince = Nothing
                              , updateStatusOptsGame = Just activity
                              , updateStatusOptsNewStatus = UpdateStatusOnline
                              , updateStatusOptsAFK = False
                              }
  sendCommand (UpdateStatus opts)

eventHandler :: Queue.Queue -> Event -> DiscordHandler ()
eventHandler queue event = case event of
      MessageCreate m -> do
        case parseMaybe commandParser (messageText m) of
          Just (Play song) -> do
            void $ restCall (R.CreateMessage (messageChannel m) ("taca-lhe pau playboy: " <> song))
            liftIO (Queue.addSong queue song)
            newQueue <- liftIO . readIORef $ queue
            void $ restCall (R.CreateMessage (messageChannel m) (Queue.print newQueue))

          Just (Remove number) -> do
            void $ restCall (R.CreateMessage (messageChannel m) "ê rural")

          _ -> liftIO $ TIO.putStrLn (messageText m)
      _ -> return ()

isTextChannel :: Channel -> Bool
isTextChannel ChannelText {} = True
isTextChannel _ = False
