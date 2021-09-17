module Cleiton
    ( pingpongExample
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

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  tok <- TIO.readFile "./auth-token.secret"

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  t <- runDiscord $ def { discordToken = tok
                        , discordOnStart = startHandler
                        , discordOnEnd = liftIO $ TIO.putStrLn "Ended"
                        , discordOnEvent = eventHandler
                        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                        }
  TIO.putStrLn t

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandler ()
startHandler = do
  Right partialGuilds <- restCall R.GetCurrentUserGuilds

  let activity = Activity { activityName = "ping-pong"
                          , activityType = ActivityTypeGame
                          , activityUrl = Nothing
                          }
  let opts = UpdateStatusOpts { updateStatusOptsSince = Nothing
                              , updateStatusOptsGame = Just activity
                              , updateStatusOptsNewStatus = UpdateStatusOnline
                              , updateStatusOptsAFK = False
                              }
  sendCommand (UpdateStatus opts)

  forM_ partialGuilds $ \pg -> do
    Right guild <- restCall $ R.GetGuild (partialGuildId pg)
    Right chans <- restCall $ R.GetGuildChannels (guildId guild)
    forM_ (take 1 (filter isTextChannel chans))
      (\channel -> restCall $ R.CreateMessage (channelId channel)
                                      "Vamo debochá legal")


-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
      MessageCreate m -> when (not (fromBot m) && isPing m) $ do
        void $ restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
        threadDelay (2 * 10^(6 :: Int))

        -- A very simple message.
        void $ restCall (R.CreateMessage (messageChannel m) "é cabeça de gelo!")

        -- A more complex message. Text-to-speech, does not mention everyone nor
        -- the user, and uses Discord native replies.
        -- Use ":info" in ghci to explore the type
        -- let opts :: R.MessageDetailedOpts
        --     opts = def { R.messageDetailedContent = "Here's a more complex message, but doesn't ping @everyone!"
        --                , R.messageDetailedTTS = True
        --                , R.messageDetailedAllowedMentions = Just $
        --                   def { R.mentionEveryone = False
        --                       , R.mentionRepliedUser = False
        --                       }
        --                , R.messageDetailedReference = Just $
        --                   def { referenceMessageId = Just $ messageId m }
        --                }
        -- void $ restCall (R.CreateMessageDetailed (messageChannel m) opts)
      _ -> return ()

isTextChannel :: Channel -> Bool
isTextChannel ChannelText {} = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("nego" `T.isPrefixOf`) . T.toLower . messageText
