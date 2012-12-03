{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, RankNTypes, RecordWildCards #-}
module Main where

import Control.Concurrent         (killThread)
import Control.Concurrent.Chan    (Chan)
import Data.Set                   (Set, insert)
import Network                    (HostName, PortID(PortNumber), connectTo)
import Network.IRC                (Message)
import Network.IRC.Bot.BotMonad   (BotMonad(..))
import Network.IRC.Bot.Core       (BotConf(..), User(..), nullBotConf, simpleBot)
import Network.IRC.Bot.Log        (LogLevel(..), nullLogger, stdoutLogger)
import Network.IRC.Bot.Part.Dice  (dicePart)
import Network.IRC.Bot.Part.Hello (helloPart)
import Network.IRC.Bot.Part.Ping  (pingPart)
import Network.IRC.Bot.Part.NickUser (nickUserPart)
import Network.IRC.Bot.Part.Channels (initChannelsPart)
import System.Console.GetOpt
import System.Environment         (getArgs, getProgName)
import System.Exit                (exitFailure)
import System.IO                  (stdout)

data Flag
    = BotConfOpt { unBotConfOpt :: (BotConf -> BotConf) }

botOpts :: [OptDescr Flag]
botOpts =
    [ Option [] ["irc-server"] (ReqArg setIrcServer "hostname or IP") "irc server to connect to"
    , Option [] ["port"]       (ReqArg setPort      "port")           "port to connect to on server"
    , Option [] ["nick"]       (ReqArg setNick      "name")           "irc nick"
    , Option [] ["username"]   (ReqArg setUsername  "username")       "ident username"
    , Option [] ["hostname"]   (ReqArg setHostname  "hostname")       "hostname of machine bot is connecting from"
    , Option [] ["realname"]   (ReqArg setRealname  "name")           "bot's real name"
    , Option [] ["cmd-prefix"] (ReqArg setCmdPrefix "prefix")         "prefix to bot commands (e.g., ?, @, bot: )"
    , Option [] ["channel"]    (ReqArg addChannel   "channel name")   "channel to join after connecting. (can be specified more than once to join multiple channels)"
    , Option [] ["log-level"]  (ReqArg setLogLevel  "debug, normal, important, quiet") "set the logging level"
    , Option [] ["limit"]      (ReqArg setLimit     "int,int")        "enable rate limiter. burst length, delay in microseconds"
    ]
    where
      setIrcServer n = BotConfOpt $ \c -> c { host = n, user = (user c) { servername = n } }
      setPort str    = BotConfOpt $ \c -> c { port = PortNumber (fromIntegral $ read str) }
      setNick n      = BotConfOpt $ \c -> c { nick = n }
      setUsername n  = BotConfOpt $ \c -> c { user = (user c) { username = n } }
      setHostname n  = BotConfOpt $ \c -> c { user = (user c) { hostname = n } }
      setRealname n  = BotConfOpt $ \c -> c { user = (user c) { realname = n } }
      setCmdPrefix p = BotConfOpt $ \c -> c { commandPrefix = p }
      addChannel ch  = BotConfOpt $ \c -> c { channels = insert ch (channels c) }
      setLogLevel l  = BotConfOpt $ \c ->
        case l of
          "debug"     -> c { logger = stdoutLogger Debug }
          "normal"    -> c { logger = stdoutLogger Normal }
          "important" -> c { logger = stdoutLogger Important }
          "quiet"     -> c { logger = nullLogger }
          _           -> error $ "unknown log-level: " ++ l
      setLimit s    = BotConfOpt $ \c ->
        case break (== ',') s of
          (burstStr, delayStr) ->
              case reads burstStr of
                [(burstLen,[])] ->
                    case reads (drop 1 $ delayStr) of
                      [(delay,[])] ->
                          c { limits = Just (burstLen, delay) }
                      _ -> error $ "unabled to parse delay: " ++ delayStr
                _ -> error $ "unabled to parse burst length: " ++ burstStr

getBotConf :: Maybe (Chan Message -> IO ()) -> IO BotConf
getBotConf mLogger =
    do args <- getArgs
       case getOpt Permute botOpts args of
         (f,_,[])   ->
             do let conf = (foldr ($) nullBotConf (map unBotConfOpt f)) { channelLogger = mLogger }
                checkConf conf
                return conf
         (_,_,errs) ->
             do progName <- getProgName
                putStr (helpMessage progName)
                exitFailure

exitHelp msg =
    do progName <- getProgName
       putStrLn msg
       putStr (helpMessage progName)
       exitFailure

checkConf :: BotConf -> IO ()
checkConf BotConf{..}
    | null host            = exitHelp "must specify --irc-server"
    | null nick            = exitHelp "must specify --nick"
    | null (username user) = exitHelp "must specify --username"
    | null (hostname user) = exitHelp "must specify --hostname"
    | null (realname user) = exitHelp "must specify --realname"
    | otherwise            = return ()

helpMessage progName = usageInfo header botOpts
  where
  header = "Usage: "++progName++" [OPTION...]\n" ++ "e.g.\n" ++
           progName ++ " --irc-server irc.freenode.net --nick stepbot --username stepbot --hostname happstack.com --realname \"happstack bot\" --channel \"#stepbot\""

main :: IO ()
main =
    do botConf <- getBotConf Nothing
       ircParts <- initParts (channels botConf)
       (tids, reconnect) <- simpleBot botConf ircParts
       (logger botConf) Important  "Press enter to force reconnect."
       getLine
       reconnect
       (logger botConf) Important  "Press enter to quit."
       getLine

       mapM_ killThread tids

initParts :: (BotMonad m) =>
             Set String  -- ^ set of channels to join
          -> IO [m ()]
initParts chans =
    do (_, channelsPart) <- initChannelsPart chans
       return [ pingPart
              , nickUserPart
              , channelsPart
              , dicePart
              , helloPart
              ]
