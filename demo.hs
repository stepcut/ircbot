{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, RankNTypes, RecordWildCards #-}
module Main where

import Control.Concurrent        (killThread)
import Control.Concurrent.Chan   (Chan)
import Network                   (HostName, PortID(PortNumber), connectTo)
import Network.IRC               (Message)
import Network.IRC.Bot.BotMonad  (BotMonad(..))
import Network.IRC.Bot.Core      (BotConf(..), User(..),simpleBot, nullBotConf)
import Network.IRC.Bot.Log       (LogLevel(..), nullLogger, stdoutLogger)
import Network.IRC.Bot.Parsec    (parsecPart)
import Network.IRC.Bot.Part.Dice (diceCommand)
import Network.IRC.Bot.Part.Ping (pingPart)
import System.Console.GetOpt
import System.Environment        (getArgs, getProgName)
import System.Exit               (exitFailure)
import System.IO                 (stdout)

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
    , Option [] ["channel"]    (ReqArg setChannel   "channel name")   "channel to join after connecting."
    , Option [] ["log-level"]  (ReqArg setLogLevel  "debug, normal, important, quiet") "set the logging level"
    ]
    where
      setIrcServer n = BotConfOpt $ \c -> c { host = n, user = (user c) { servername = n } }
      setPort str    = BotConfOpt $ \c -> c { port = Just $ PortNumber (fromIntegral $ read str) }
      setNick n      = BotConfOpt $ \c -> c { nick = n }
      setUsername n  = BotConfOpt $ \c -> c { user = (user c) { username = n } }
      setHostname n  = BotConfOpt $ \c -> c { user = (user c) { hostname = n } }
      setRealname n  = BotConfOpt $ \c -> c { user = (user c) { realname = n } }
      setChannel ch  = BotConfOpt $ \c -> c { channel = ch }
      setLogLevel l  = BotConfOpt $ \c ->
        case l of
          "debug"     -> c { logger = stdoutLogger Debug }
          "normal"    -> c { logger = stdoutLogger Normal }
          "important" -> c { logger = stdoutLogger Important }
          "quiet"     -> c { logger = nullLogger }


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
    | null channel         = exitHelp "must specify --channel"
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
       tids <- simpleBot botConf ircParts
       (logger botConf) Important  "Press enter to quit."
       getLine
       mapM_ killThread tids

ircParts :: (BotMonad m) => [m ()]
ircParts = 
  [ pingPart
  , parsecPart "stepbot" diceCommand
  ]
