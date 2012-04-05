{-# LANGUAGE FlexibleContexts #-}
module Network.IRC.Bot.Parsec where

{-

The parsec part is supposed to make it easy to use Parsec to parse the command arguments.

We would also like to be able to generate a help menu. But the help
menu should not be for only Parsec commands. Or do we? Maybe all interactive commands should be implementing through parsec part.

Some commands like @seen (and @tell) are two part. There is the part that collects
the data. And there is the command itself. How would that integrate
with a parsec command master list?

We would like the parsec commands to be non-blocking.

Each top-level part is run in a separate thread. But if we only have one thread for all the parsecParts, then blocking could occur.

We could run every handler for every message, even though we only expect at most one command to match. That seems bogus. Do we really want to allow to different parts to respond to @foo ?

Seems better to have each part register.

data Part m =
    Part { name            :: String
         , description     :: String
         , backgroundParts :: [BotPartT m ()]
         , command         :: Maybe (String, String, BotPartT m ()) -- ^ (name, usage, handler)
         }

This is good, unless multiple plugins wanted to depend on some common backgroundParts
-}

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans
import Data.Char (digitToInt)
import Data.List (intercalate, isPrefixOf, nub)
import Data.Maybe (fromMaybe)
import Network.IRC.Bot.Log
import Network.IRC.Bot.BotMonad
import Network.IRC.Bot.Commands
import Text.Parsec
import Text.Parsec.Error (errorMessages, messageString)
import qualified Text.Parsec.Error as P

instance (BotMonad m, Monad m) => BotMonad (ParsecT s u m) where
    askBotEnv        = lift askBotEnv
    askMessage       = lift askMessage
    askOutChan       = lift askOutChan
    localMessage f m = mapParsecT (localMessage f) m
    sendMessage      = lift . sendMessage
    logM lvl msg     = lift (logM lvl msg)
    whoami           = lift whoami

mapParsecT :: (Monad m, Monad n) => (m (Consumed (m (Reply s u a))) -> n (Consumed (n (Reply s u b)))) -> ParsecT s u m a -> ParsecT s u n b
mapParsecT f p = mkPT $ \s -> f (runParsecT p s)

-- | parse a positive integer
nat :: (Monad m) => ParsecT String () m Integer
nat =
    do digits <- many1 digit
       return $ foldl (\x d -> x * 10 + fromIntegral (digitToInt d)) 0 digits

-- | parser that checks for the 'cmdPrefix' (from the 'BotEnv')
botPrefix :: (BotMonad m) => ParsecT String () m ()
botPrefix =
    do recv <- fromMaybe "" <$> askReceiver
       pref <- cmdPrefix <$> askBotEnv
       if "#" `isPrefixOf` recv
          then (try $ string pref >> return ()) <|> lift mzero
          else (try $ string pref >> return ()) <|> return ()

-- | create a bot part by using Parsec to parse the command
--
-- The argument to 'parsecPart' is a parser function.
--
-- The argument to that parsec function is the 'target' that the response should be sent to.
--
-- The parser will receive the 'msg' from the 'PrivMsg'.
--
-- see 'dicePart' for an example usage.
parsecPart :: (BotMonad m) =>
              (ParsecT String () m a)
           -> m a
parsecPart p =
    do priv <- privMsg
       logM Debug $ "I got a message: " ++ msg priv ++ " sent to " ++ show (receivers priv)
       ma <- runParserT p () (msg priv) (msg priv)
       case ma of
         (Left e) ->
             do logM Debug $ "Parse error: " ++ show e
                target <- maybeZero =<< replyTo
                reportError target e
                mzero
         (Right a) -> return a

reportError :: (BotMonad m) => String -> ParseError -> m ()
reportError target err =
    let errStrs = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages err)
        errStr = intercalate "; " errStrs
    in sendCommand (PrivMsg Nothing [target] errStr)

showErrorMessages ::
    String -> String -> String -> String -> String -> [P.Message] -> [String]
showErrorMessages msgOr msgUnknown msgExpecting msgUnExpected msgEndOfInput msgs
    | null msgs = [msgUnknown]
    | otherwise = clean $
                 [showSysUnExpect,showUnExpect,showExpect,showMessages]
    where
      (sysUnExpect,msgs1) = span ((P.SysUnExpect "") ==) msgs
      (unExpect,msgs2)    = span ((P.UnExpect    "") ==) msgs1
      (expect,messages)   = span ((P.Expect      "") ==) msgs2

      showExpect      = showMany msgExpecting expect
      showUnExpect    = showMany msgUnExpected unExpect
      showSysUnExpect | not (null unExpect) ||
                        null sysUnExpect = ""
                      | null firstMsg    = msgUnExpected ++ " " ++ msgEndOfInput
                      | otherwise        = msgUnExpected ++ " " ++ firstMsg
          where
              firstMsg  = messageString (head sysUnExpect)

      showMessages      = showMany "" messages

      -- helpers
      showMany pre msgs = case clean (map messageString msgs) of
                            [] -> ""
                            ms | null pre  -> commasOr ms
                               | otherwise -> pre ++ " " ++ commasOr ms

      commasOr []       = ""
      commasOr [m]      = m
      commasOr ms       = commaSep (init ms) ++ " " ++ msgOr ++ " " ++ last ms

      commaSep          = seperate ", " . clean

      seperate   _ []     = ""
      seperate   _ [m]    = m
      seperate sep (m:ms) = m ++ sep ++ seperate sep ms

      clean             = nub . filter (not . null)
