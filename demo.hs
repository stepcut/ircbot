{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, RankNTypes, RecordWildCards #-}
module Main where

import Control.Concurrent        (killThread)
import Control.Concurrent.Chan   (Chan)
import Network                   (HostName, PortID(PortNumber), connectTo)
import Network.IRC               (Message)
import Network.IRC.Bot.BotMonad  (BotMonad(..))
import Network.IRC.Bot.Core      (BotConf(..), User(..),simpleBot, nullBotConf)
import Network.IRC.Bot.Parsec    (parsecPart)
import Network.IRC.Bot.Part.Dice (diceCommand)
import Network.IRC.Bot.Part.Ping (pingPart)
import System.Console.GetOpt
import System.Environment        (getArgs, getProgName)
import System.Exit               (exitFailure)
import System.IO                 (stdout)

botOpts :: [OptDescr (BotConf -> BotConf)]
botOpts = 
    [ Option [] ["irc-server"] (ReqArg setIrcServer "hostname or IP") "irc server to connect to" 
    , Option [] ["port"]       (ReqArg setPort      "port")           "port to connect to on server"
    , Option [] ["nick"]       (ReqArg setNick      "name")           "irc nick"
    , Option [] ["username"]   (ReqArg setUsername  "username")       "ident username"
    , Option [] ["hostname"]   (ReqArg setHostname  "hostname")       "hostname of machine bot is connecting from"
    , Option [] ["realname"]   (ReqArg setRealname  "name")           "bot's real name"
    , Option [] ["channel"]    (ReqArg setChannel   "channel name")   "channel to join after connecting."
    ]
    where
      setIrcServer n c = c { host = n, user = (user c) { servername = n } }
      setPort str c    = c { port = Just $ PortNumber (fromIntegral $ read str) }
      setNick n c      = c { nick = n }
      setUsername n c  = c { user = (user c) { username = n } }
      setHostname n c  = c { user = (user c) { hostname = n } }
      setRealname n c  = c { user = (user c) { realname = n } }
      setChannel ch c  = c { channel = ch }

getBotConf :: Maybe (Chan Message -> IO ()) -> IO BotConf 
getBotConf mLogger =
    do args <- getArgs
       case getOpt Permute botOpts args of
         (f,_,[])   -> 
             do let conf = (foldr ($) nullBotConf f) { logger = mLogger }
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
--       tids <- simpleBot Nothing "irc.freenode.net" (PortNumber 6667) "stepbot" (User "stepbot" "happstack.com" "irc.freenode.net" "happstack bot") "#stepbot" ircParts
       tids <- simpleBot botConf ircParts
       putStrLn "Press enter to quit."
       getLine
       mapM_ killThread tids

ircParts :: (BotMonad m) => [m ()]
ircParts = 
  [ pingPart
  , parsecPart "stepbot" diceCommand
  ]
