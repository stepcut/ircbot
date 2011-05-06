module Network.IRC.Bot.Part.Dice where

import Control.Monad
import Control.Monad.Trans
import Network.IRC.Bot.Log
import Network.IRC.Bot.BotMonad
import Network.IRC.Bot.Commands
import Network.IRC.Bot.Parsec
import System.Random (randomRIO)
import Text.Parsec
import Text.Parsec.Error (errorMessages, messageString)
import qualified Text.Parsec.Error as P

dicePart :: (BotMonad m) => m ()
dicePart = parsecPart diceCommand

diceCommand :: (BotMonad m) => String -> ParsecT String () m ()
diceCommand target =
    do try $ string "dice"
       (numDice, numSides, modifier) <- (do 
         skipMany1 space
         nd <- nat <|> return 1
         if nd > 100
            then fail "You can not roll more than 100 dice."
            else do
              char 'd'
              ns <- (do n <- nat
                        if n > 0
                         then return n
                         else fail "The dice must have at least 1 side"
                    )
              mod <- (do char '+' >> nat) <|> return 0
              return (nd, ns, mod)) <?> "dice <num-dice>d<num-sides>[+<modifier>]"
       rolls <- liftIO $ replicateM (fromIntegral numDice) $ randomRIO (1, numSides)
       let results = "You rolled " ++ show numDice ++ " " ++ show numSides ++ "-sided dice with a +" ++ show modifier ++ " modifier: " ++ show rolls ++ " => " ++ show (sum (modifier : rolls))
       sendCommand (PrivMsg Nothing [target] results)
    <|> return ()
