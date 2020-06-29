{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Bot.Options
  ( execBotOptsParser
  , parseBotConf
  ) where

import Data.ByteString            (ByteString)

import qualified Data.ByteString.Char8
import qualified Data.Set

import Options.Applicative

import Network.IRC.Bot.Core       (BotConf(..), User(..))
import Network.IRC.Bot.Log        (Logger, LogLevel(..), stdoutLogger)

execBotOptsParser :: Parser extra -> IO (BotConf, extra)
execBotOptsParser parseExtra = execParser
  $ info (((,) <$> parseBotConf <*> parseExtra) <**> helper)
    ( fullDesc
    <> progDesc "ircbot"
    <> header "ircbot - Haskell IRC bot" )

parseBotConf :: Parser BotConf
parseBotConf = BotConf
  <$> pure Nothing
  <*> parseLogger
  <*> strOption
    (long "server" 
     <> short 's'
     <> metavar "HOST_OR_IP"
     <> value "localhost"
     <> help "IRC server to connect to")
  <*> option auto
    (long "port"
     <> short 'p'
     <> metavar "PORT"
     <> value 6667
     <> help "Port of the IRC server to use")
  <*> strOption
    (long "nick"
     <> short 'n'
     <> help "IRC nick")
  <*> strOption
    (long "cmd-prefix"
     <> short 'c'
     <> value "#"
     <> help "Bot command prefix")
  <*> parseUser
  <*> (Data.Set.fromList . map coercePrefixes <$>
     some (argument str
             (metavar "CHANNEL [CHANNEL]"
             <> help "IRC channels to join to, channel prefix # not required")
            ))
  <*> parseLimits

-- | Prefix channel name with '#' if needed
coercePrefixes :: ByteString -> ByteString
coercePrefixes x | "#" `Data.ByteString.Char8.isPrefixOf` x = x
coercePrefixes x | otherwise = Data.ByteString.Char8.cons '#' x

parseLogger :: Parser Logger
parseLogger = stdoutLogger
  <$> flag Normal Debug
    (long "debug"
     <> short 'd'
     <> help "Enable debug output")

parseUser :: Parser User
parseUser = User
  <$> strOption
    (long "username"
     <> help "Ident username")
  <*> strOption
    (long "hostname"
     <> help "Hostname of the client system")
  <*> pure "."
  <*> strOption
    (long "realname"
     <> help "Clients real name")

parseLimits :: Parser (Maybe (Int, Int))
parseLimits = optional $ (,)
  <$> option auto
    (long "burst-length"
     <> metavar "BURST"
     <> value 2
     <> help "Rate limit after a BURST limit of messages is reached")
  <*> option auto
    (long "delay-ms"
     <> metavar "MS"
     <> value 2
     <> help "Delay in microseconds for rate limiting")
