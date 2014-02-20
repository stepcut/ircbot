{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
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
import Control.Concurrent.STM   (atomically)
import Control.Concurrent.STM.TMVar  (TMVar, swapTMVar, newTMVar, readTMVar)
import Control.Exception        (IOException, catch)
import Control.Monad            (mplus, forever, when)
import Control.Monad.Trans      (liftIO)
import Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Data                (Data, Typeable)
import Data.Monoid              ((<>))
import Data.Set                 (Set, empty)
import Data.Time                (UTCTime, addUTCTime, getCurrentTime)
import GHC.IO.Handle            (hFlushAll)
import Network                  (HostName, PortID(PortNumber), connectTo)
import Network.IRC              (Message, decode, encode, joinChan, nick, showMessage, user)
import Network.IRC              as I
import Network.IRC.Bot.Types    (User(..), nullUser)
import Network.IRC.Bot.Limiter  (Limiter(..), newLimiter, limit)
import Network.IRC.Bot.Log      (Logger, LogLevel(Normal, Debug), stdoutLogger)
import Network.IRC.Bot.BotMonad (BotMonad(logM, sendMessage), BotPartT, BotEnv(..), runBotPartT)
import Network.IRC.Bot.Part.NickUser (changeNickUser)
import Prelude                  hiding (catch)
import           Control.Concurrent.SSem (SSem)
import qualified Control.Concurrent.SSem as SSem
import System.IO                (BufferMode(NoBuffering, LineBuffering), Handle, hClose, hGetLine, hPutChar, hSetBuffering)

-- |Bot configuration
data BotConf =
    BotConf
    { channelLogger :: (Maybe (Chan Message -> IO ()))  -- ^ optional channel logging function
    , logger        :: Logger           -- ^ app logging
    , host          :: HostName         -- ^ irc server to connect
    , port          :: PortID           -- ^ irc port to connect to (usually, 'PortNumber 6667')
    , nick          :: ByteString       -- ^ irc nick
    , commandPrefix :: String           -- ^ command prefix
    , user          :: User             -- ^ irc user info
    , channels      :: Set ByteString   -- ^ channel to join
    , limits        :: Maybe (Int, Int) -- ^ (burst length, delay in microseconds)
    }

nullBotConf :: BotConf
nullBotConf =
    BotConf { channelLogger  = Nothing
            , logger         = stdoutLogger Normal
            , host           = ""
            , port           = PortNumber 6667
            , nick           = ""
            , commandPrefix  = "#"
            , user           = nullUser
            , channels       = empty
            , limits         = Nothing
            }

-- | connect to irc server and send NICK and USER commands
ircConnect :: HostName
           -> PortID
           -> ByteString
           -> User
           -> IO Handle
ircConnect host port n u =
    do h <- connectTo host port
       hSetBuffering h LineBuffering
       return h

partLoop :: Logger -> ByteString -> String -> Chan Message -> Chan Message -> (BotPartT IO ()) -> IO ()
partLoop logger botName prefix incomingChan outgoingChan botPart =
  forever $ do msg <- readChan incomingChan
               runBotPartT botPart (BotEnv msg outgoingChan logger botName prefix)

ircLoop :: Logger -> ByteString -> String -> Chan Message -> Chan Message -> [BotPartT IO ()] -> IO [ThreadId]
ircLoop logger botName prefix incomingChan outgoingChan parts =
    mapM forkPart parts
  where
    forkPart botPart =
      do inChan <- dupChan incomingChan
         forkIO $ partLoop logger botName prefix inChan outgoingChan (botPart `mplus` return ())

-- reconnect loop is still a bit buggy
-- if you try to write multiple lines, and the all fail, reconnect will be called multiple times..
-- something should be done so that this does not happen
connectionLoop :: Logger -> Maybe (Int, Int) -> TMVar UTCTime -> HostName -> PortID -> ByteString -> User -> Chan Message -> Chan Message -> Maybe (Chan Message) -> SSem -> IO (ThreadId, ThreadId, Maybe ThreadId, IO ())
connectionLoop logger mLimitConf tmv host port nick user outgoingChan incomingChan logChan connSSem =
  do hTMVar <- atomically $ newTMVar (undefined :: Handle)
     (limit, limitTid) <-
         case mLimitConf of
           Nothing -> return (return (), Nothing)
           (Just (burst, delay)) ->
                    do limiter <- newLimiter burst delay
                       return (limit limiter, Just $ limitsThreadId limiter)
     outgoingTid  <- forkIO $ forever $
                      do msg <- readChan outgoingChan
                         writeMaybeChan logChan msg
                         h <- atomically $ readTMVar hTMVar
                         when (msg_command msg `elem` ["PRIVMSG", "NOTICE"]) limit
                         C.hPutStr h (encode msg) `catch` (reconnect logger host port nick user hTMVar connSSem)
                         hPutChar h '\n'
                         now <- getCurrentTime
                         atomically $ swapTMVar tmv now
     incomingTid  <- forkIO $ do
                      doConnect logger host port nick user hTMVar connSSem
                      forever $
                       do h <- atomically $ readTMVar hTMVar
                          -- FIXME: is C.hGetLine going to do the write thing in the face of unicode?
                          msgStr <- (C.hGetLine h) `catch` (\e -> reconnect logger host port nick user hTMVar connSSem e >> return "")
                          now <- getCurrentTime
                          atomically $ swapTMVar tmv now
                          case decode (msgStr <> "\n") of
                            Nothing -> logger Normal ("decode failed: " <> msgStr)
                            (Just msg) ->
                              do logger Debug (showMessage msg)
                                 writeMaybeChan logChan msg
                                 writeChan incomingChan msg
     let forceReconnect =
             do putStrLn "forceReconnect: getting handle"
                h <- atomically $ readTMVar hTMVar
                putStrLn "forceReconnect: sending /quit"
                writeChan outgoingChan (quit $ Just "restarting...")
                putStrLn "forceReconnect: closing handle"
                hClose h
                putStrLn "done."
     return (outgoingTid, incomingTid, limitTid, forceReconnect)

ircConnectLoop :: (LogLevel -> ByteString -> IO a) -- ^ logging
               -> HostName
               -> PortID
               -> ByteString
               -> User
               -> IO Handle
ircConnectLoop logger host port nick user =
        (ircConnect host port nick user) `catch`
        (\e ->
          do logger Normal $ "irc connect failed ... retry in 60 seconds: " <> (C.pack $ show (e :: IOException))
             threadDelay (60 * 10^6)
             ircConnectLoop logger host port nick user)

doConnect :: (LogLevel -> ByteString -> IO a) -> HostName -> PortID -> ByteString -> User -> TMVar Handle -> SSem -> IO ()
doConnect logger host port nick user hTMVar connSSem =
    do logger Normal $ "Connecting to " <> (C.pack host) <> " as " <> nick
       h <- ircConnectLoop logger host port nick user
       atomically $ swapTMVar hTMVar h
       logger Normal $ "Connected."
       SSem.signal connSSem
       return ()

reconnect :: Logger -> HostName -> PortID -> ByteString -> User -> TMVar Handle -> SSem -> IOException -> IO ()
reconnect logger host port nick user hTMVar connSSem e =
    do logger Normal $ "IRC Connection died: " <> C.pack (show e)
{-
       atomically $ do empty <- isEmptyTMVar hTMVar
                       if empty
                          then return ()
                          else takeTMVar hTMVar >> return ()
-}
       doConnect logger host port nick user hTMVar connSSem

onConnectLoop :: Logger -> ByteString -> String -> Chan Message -> SSem -> BotPartT IO () -> IO ThreadId
onConnectLoop logger botName prefix outgoingChan connSSem action =
    forkIO $ forever $
      do SSem.wait connSSem
         runBotPartT action (BotEnv undefined outgoingChan logger botName prefix)

-- |simpleBot connects to the server and handles messages using the supplied BotPartTs
--
-- the 'Chan Message' for the optional logging function will include
-- all received and sent messages. This means that the bots output
-- will be included in the logs.
simpleBot :: BotConf          -- ^ Bot configuration
          -> [BotPartT IO ()] -- ^ bot parts (must include 'pingPart', or equivalent)
          -> IO ([ThreadId], IO ())    -- ^ 'ThreadId' for all forked handler threads and a function that forces a reconnect
simpleBot BotConf{..} parts =
    simpleBot' channelLogger logger limits host port nick commandPrefix user parts

-- |simpleBot' connects to the server and handles messages using the supplied BotPartTs
--
-- the 'Chan Message' for the optional logging function will include
-- all received and sent messages. This means that the bots output
-- will be included in the logs.
simpleBot' :: (Maybe (Chan Message -> IO ())) -- ^ optional logging function
          -> Logger           -- ^ application logging
          -> Maybe (Int, Int) -- ^ rate limiter settings (burst length, delay in microseconds)
          -> HostName         -- ^ irc server to connect
          -> PortID           -- ^ irc port to connect to (usually, 'PortNumber 6667')
          -> ByteString       -- ^ irc nick
          -> String           -- ^ command prefix
          -> User             -- ^ irc user info
          -> [BotPartT IO ()] -- ^ bot parts (must include 'pingPart', 'channelsPart', and 'nickUserPart)'
          -> IO ([ThreadId], IO ())    -- ^ 'ThreadId' for all forked handler threads and an IO action that forces a reconnect
simpleBot' mChanLogger logger limitConf host port nick prefix user parts =
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
     now <- getCurrentTime
     tmv <- atomically $ newTMVar now
     connSSem <- SSem.new 0
     (outgoingTid, incomingTid, mLimitTid, forceReconnect) <- connectionLoop logger limitConf tmv host port nick user outgoingChan incomingChan mLogChan connSSem
     watchDogTid <- forkIO $ forever $
                    do let timeout = 5*60
                       now          <- getCurrentTime
                       lastActivity <- atomically $ readTMVar tmv
                       when (now > addUTCTime (fromIntegral timeout) lastActivity) forceReconnect
                       threadDelay (30*10^6) -- check every 30 seconds
     ircTids     <- ircLoop logger nick prefix incomingChan outgoingChan parts
     onConnectId <- onConnectLoop logger nick prefix outgoingChan connSSem onConnect
     return $ (maybe id (:) mLimitTid $ maybe id (:) mLogTid $ (incomingTid : outgoingTid : watchDogTid : ircTids), forceReconnect)
    where
      onConnect :: BotPartT IO ()
      onConnect =
          changeNickUser nick (Just user)

-- | call 'writeChan' if 'Just'. Do nothing for Nothing.
writeMaybeChan :: Maybe (Chan a) -> a -> IO ()
writeMaybeChan Nothing     _ = return ()
writeMaybeChan (Just chan) a = writeChan chan a
