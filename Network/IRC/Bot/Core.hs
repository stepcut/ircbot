{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Network.IRC.Bot.Core 
    ( simpleBot
    , simpleBot'
    , BotConf(..)
    , nullBotConf
    , User(..)
    , nullUser
    ) where

import Control.Concurrent       (ThreadId, forkIO, threadDelay)
import Control.Concurrent.Chan  (Chan, dupChan, newChan, readChan, writeChan)
import Control.Concurrent.MVar  (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception        (IOException, catch)
import Control.Monad            (mplus, forever, when)
import Data.Data                (Data, Typeable)
import Data.Maybe               (fromMaybe)
import Data.Time                (UTCTime, addUTCTime, getCurrentTime)
import Network                  (HostName, PortID(PortNumber), connectTo)
import Network.IRC              (Message, decode, encode, joinChan, nick, user)
import Network.IRC              as I
import Network.IRC.Bot.Log      (Logger, LogLevel(Normal, Debug), stdoutLogger)
import Network.IRC.Bot.BotMonad (BotMonad(logM), BotPartT, BotEnv(..), runBotPartT)
import Prelude                  hiding (catch)
import System.IO                (BufferMode(LineBuffering), Handle, hClose, hGetLine, hPutStrLn, hSetBuffering)

-- |Bot configuration
data BotConf = 
    BotConf
    { channelLogger :: (Maybe (Chan Message -> IO ()))  -- ^ optional channel logging function
    , logger :: Logger           -- ^ app logging
    , host   :: HostName         -- ^ irc server to connect 
    , port   :: Maybe PortID     -- ^ irc port to connect to (usually, 'PortNumber 6667')
    , nick   :: String           -- ^ irc nick
    , user   :: User             -- ^ irc user info
    , channel :: String          -- ^ channel to join
    }

nullBotConf :: BotConf
nullBotConf =
    BotConf { channelLogger  = Nothing
            , logger  = stdoutLogger Normal
            , host    = ""
            , port    = Nothing
            , nick    = ""
            , user    = nullUser
            , channel = ""
            }

data User = User
    { username   :: String    -- ^ username on client system
    , hostname   :: HostName  -- ^ hostname of client system
    , servername :: HostName  -- ^ irc server client is connected to
    , realname   :: String    -- ^ client's real name
    }
    deriving (Data, Typeable, Eq, Ord, Read, Show)

nullUser :: User
nullUser = User { username   = ""
                , hostname   = ""
                , servername = ""
                , realname   = ""
                }

-- | connect to irc server and send NICK and USER commands
ircConnect :: HostName -> PortID -> String -> User -> IO Handle
ircConnect host port n u =
    do h <- connectTo host port
       hSetBuffering h LineBuffering
       hPutStrLn h (encode (I.nick n))
       hPutStrLn h (encode (I.user (username u) (hostname u) (servername u) (realname u)))
       return h
       
partLoop :: Logger -> Chan Message -> Chan Message -> (BotPartT IO ()) -> IO ()
partLoop logger incomingChan outgoingChan botPart =
  forever $ do msg <- readChan incomingChan
               runBotPartT botPart (BotEnv msg outgoingChan logger)
               
ircLoop :: Logger -> Chan Message -> Chan Message -> [BotPartT IO ()] -> IO [ThreadId]
ircLoop logger incomingChan outgoingChan parts = mapM forkPart parts
  where
    forkPart botPart =
      do inChan <- dupChan incomingChan
         forkIO $ partLoop logger inChan outgoingChan (botPart `mplus` return ())
       
-- reconnect loop is still a bit buggy     
-- if you try to write multiple lines, and the all fail, reconnect will be called multiple times..
-- something should be done so that this does not happen
connectionLoop :: Logger -> MVar UTCTime -> HostName -> PortID -> String -> User -> Chan Message -> Chan Message -> Maybe (Chan Message) -> IO () -> IO (ThreadId, ThreadId, IO ())
connectionLoop logger mv host port nick user outgoingChan incomingChan logChan onConnect =
  do hMVar <- newMVar (undefined :: Handle)
     doConnect logger host port nick user onConnect hMVar
     outgoingTid  <- forkIO $ forever $
                      do msg <- readChan outgoingChan
                         writeMaybeChan logChan msg
                         h <- readMVar hMVar
                         hPutStrLn h (encode msg) `catch` (reconnect logger host port nick user onConnect hMVar)
                         modifyMVar_ mv (const getCurrentTime) 
     incomingTid  <- forkIO $ forever $
                       do h <- readMVar hMVar
                          msgStr <- (hGetLine h) `catch` (\e -> reconnect logger host port nick user onConnect hMVar e >> return "")
                          modifyMVar_ mv (const getCurrentTime)
                          case decode (msgStr ++ "\n") of
                            Nothing -> logger Normal ("decode failed: " ++ msgStr)
                            (Just msg) -> 
                              do logger Debug (show msg)
                                 writeMaybeChan logChan msg
                                 writeChan incomingChan msg
     let forceReconnect = 
             do h <- readMVar hMVar
                hClose h
     return (outgoingTid, incomingTid, forceReconnect)

 
ircConnectLoop logger host port nick user =
        (ircConnect host port nick user) `catch` 
        (\e ->
          do logger Normal $ "irc connect failed ... retry in 60 seconds: " ++ show (e :: IOException)
             threadDelay (60 * 10^6)
             ircConnectLoop logger host port nick user)
 
doConnect logger host port nick user onConnect hMVar =
    do logger Normal $ showString "Connecting to " . showString host . showString " as " $ nick
       h <- ircConnectLoop logger host port nick user
       modifyMVar_ hMVar (const $ return h)
       logger Normal $ "Connected."
       onConnect
       return ()

reconnect :: Logger -> String -> PortID -> String -> User -> IO a -> MVar Handle -> IOException -> IO ()
reconnect logger host port nick user onConnect hMVar e = 
    do logger Normal $ "IRC Connection died: " ++ show e
       doConnect logger host port nick user onConnect hMVar

-- |simpleBot connects to the server and handles messages using the supplied BotPartTs
-- 
-- the 'Chan Message' for the optional logging function will include
-- all received and sent messages. This means that the bots output
-- will be included in the logs.
simpleBot :: BotConf          -- ^ Bot configuration
          -> [BotPartT IO ()] -- ^ bot parts (must include 'pingPart', or equivalent)
          -> IO [ThreadId]    -- ^ 'ThreadId' for all forked handler threads
simpleBot BotConf{..} parts =
    simpleBot' channelLogger logger host port nick user channel parts

-- |simpleBot' connects to the server and handles messages using the supplied BotPartTs
--
-- the 'Chan Message' for the optional logging function will include
-- all received and sent messages. This means that the bots output
-- will be included in the logs.
simpleBot' :: (Maybe (Chan Message -> IO ())) -- ^ optional logging function
          -> Logger           -- ^ application logging
          -> HostName         -- ^ irc server to connect 
          -> Maybe PortID           -- ^ irc port to connect to (usually, 'PortNumber 6667')
          -> String           -- ^ irc nick
          -> User             -- ^ irc user info
          -> String           -- ^ channel to join
          -> [BotPartT IO ()] -- ^ bot parts (must include 'pingPart', or equivalent)
          -> IO [ThreadId]    -- ^ 'ThreadId' for all forked handler threads
simpleBot' mChanLogger logger host mPort nick user channel parts =  
  do (mLogTid, mLogChan) <- 
         case mChanLogger of
           Nothing  -> return (Nothing, Nothing)
           (Just chanLogger) ->
               do logChan <- newChan :: IO (Chan Message)
                  logTid  <- forkIO $ chanLogger logChan
                  return (Just logTid, Just logChan)
     -- message channels
     outgoingChan <- newChan :: IO (Chan Message)
     incomingChan <- newChan :: IO (Chan Message)
     mv <- newMVar =<< getCurrentTime
     (outgoingTid, incomingTid, forceReconnect) <- connectionLoop logger mv host (fromMaybe (PortNumber 6667) mPort) nick user outgoingChan incomingChan mLogChan (onConnect outgoingChan)
     watchDogTid <- forkIO $ forever $ 
                    do let timeout = 5*60
                       now          <- getCurrentTime
                       lastActivity <- readMVar mv
                       when (now > addUTCTime (fromIntegral timeout) lastActivity) forceReconnect
                       threadDelay (30*10^6) -- check every 30 seconds
     ircTids <- ircLoop logger incomingChan outgoingChan parts
     return $ maybe id (:) mLogTid $ (incomingTid : outgoingTid : watchDogTid : ircTids)
    where
      onConnect outgoingChan = 
        do logger Normal $ "joining channel " ++ channel
           writeChan outgoingChan (joinChan channel)


-- | call 'writeChan' if 'Just'. Do nothing for Nothing.
writeMaybeChan :: Maybe (Chan a) -> a -> IO ()
writeMaybeChan Nothing     _ = return () 
writeMaybeChan (Just chan) a = writeChan chan a