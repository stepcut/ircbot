{-# LANGUAGE OverloadedStrings #-}
module Network.IRC.Bot.Part.Dice where

import Control.Monad            (replicateM, void)
import Control.Monad.Trans      (liftIO)
import Data.ByteString          (ByteString)
import Data.ByteString.Char8    (pack)
import Network.IRC.Bot.Log      (LogLevel(Debug))
import Network.IRC.Bot.BotMonad (BotMonad(..), maybeZero)
import Network.IRC.Bot.Commands (PrivMsg(..), sendCommand, replyTo)
import Network.IRC.Bot.Parsec   (botPrefix, nat, parsecPart)
import System.Random            (randomRIO)
import Text.Parsec              (ParsecT, (<|>), (<?>), char, skipMany1, space, string, try)

dicePart :: (BotMonad m) => m ()
dicePart = parsecPart diceCommand

diceCommand :: (BotMonad m) => ParsecT ByteString () m ()
diceCommand =
    do void $ try $ botPrefix >> string "dice"
       logM Debug "dicePart"
       target <- maybeZero =<< replyTo
       (numDice, numSides, modifier) <- (do
         skipMany1 space
         nd <- nat <|> return 1
         if nd > 100
            then fail "You can not roll more than 100 dice."
            else do
              _ <- char 'd'
              ns <- (do n <- nat
                        if n > 0
                         then return n
                         else fail "The dice must have at least 1 side"
                    )
              modifier <- (do char '+' >> nat) <|> return 0
              return (nd, ns, modifier)) <?> "dice <num-dice>d<num-sides>[+<modifier>]"
       rolls <- liftIO $ replicateM (fromIntegral numDice) $ randomRIO (1, numSides)
       let results = "You rolled " ++ show numDice ++ " " ++ show numSides ++ "-sided dice with a +" ++ show modifier ++ " modifier: " ++ show rolls ++ " => " ++ show (sum (modifier : rolls))
       sendCommand (PrivMsg Nothing [target] (pack results))
    <|> return ()
