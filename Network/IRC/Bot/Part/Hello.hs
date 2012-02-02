module Network.IRC.Bot.Part.Hello where

import Control.Monad.Trans      (liftIO)
import Data.Maybe               (fromMaybe)
import Network.IRC.Bot.Log      (LogLevel(Debug))
import Network.IRC.Bot.BotMonad (BotMonad(..), maybeZero)
import Network.IRC.Bot.Commands (PrivMsg(..),askSenderNickName, replyTo, sendCommand)
import Network.IRC.Bot.Parsec   (botPrefix, parsecPart)
import System.Random            (randomRIO)
import Text.Parsec              (ParsecT, (<|>), string, try)

helloPart :: (BotMonad m) => m ()
helloPart = parsecPart helloCommand

helloCommand :: (BotMonad m) => ParsecT String () m ()
helloCommand =
    do try $ botPrefix >> string "hello"
       logM Debug "helloPart"
       target <- maybeZero =<< replyTo
       logM Debug $ "target: " ++ target
       mNick <- askSenderNickName
       let greetings = ["Hello", "Howdy", "Greetings", "Word up"]
       n <- liftIO $ randomRIO (0, length greetings - 1)
       let msg = greetings!!n ++ ", " ++ (fromMaybe "stranger" mNick)
       sendCommand (PrivMsg Nothing [target] msg)
    <|> return ()
