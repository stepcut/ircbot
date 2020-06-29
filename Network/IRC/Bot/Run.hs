-- {-# LANGUAGE OverloadedStrings #-}

-- | Simple wrapper on top of simpleBot
-- adding applicative parser
-- and `runBotWithParts` shorthand.
--
-- `runBotWithParts` allows passing
-- initialization function that inits
-- all bot parts and returns them as list.

module Network.IRC.Bot.Run (
    runBotWithParts
  ) where

import Data.ByteString            (ByteString)
import Data.Set                   (Set)
import Options.Applicative        (Parser)

import qualified Control.Concurrent
import qualified Control.Monad
import qualified Data.List
import qualified System.IO

import Network.IRC.Bot.BotMonad   (BotMonad(..), BotPartT)
import Network.IRC.Bot.Core       (BotConf(..), simpleBot)
import Network.IRC.Bot.Options    (execBotOptsParser)
import Network.IRC.Bot.Part.Channels (initChannelsPart)
import Network.IRC.Bot.Part.Ping     (pingPart)


-- | Run bot with user provided initialization
-- function returning bot parts.
runBotWithParts :: IO [BotPartT IO ()] -> IO ()
runBotWithParts parts = runBotWithParts' (pure ()) (const parts)

-- | Run bot with user provided initialization
-- function returning bot parts.
--
-- Accepts another `optparse-applicative` `Parser` for extending
-- built-in one.
runBotWithParts' :: Parser extra
                -> (extra -> IO [BotPartT IO ()])
                -> IO ()
runBotWithParts' extrasParser initUserParts = do
  (botOptions, extras) <- execBotOptsParser extrasParser

  ircParts <- initParts (initUserParts extras) (channels botOptions)
  (tids, reconnect) <- simpleBot botOptions ircParts
  hasStdin <- System.IO.isEOF
  case hasStdin of
    True -> Control.Monad.forever $ Control.Concurrent.threadDelay 1000000000
    False -> do
      let loop = do
            l <- getLine
            Control.Monad.unless ("quit" `Data.List.isPrefixOf` l) $ do
              reconnect
              loop

      loop
  mapM_ Control.Concurrent.killThread tids

-- Init channels part and all user parts
initParts :: (BotMonad m)
          => (IO [m ()])     -- ^ User provided parts
          -> Set ByteString  -- ^ Set of channels to join
          -> IO [m ()]
initParts initUser chans = do
  (_, channelsPart) <- initChannelsPart chans
  userParts <- initUser
  return $ channelsPart:pingPart:userParts
