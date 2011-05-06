module Network.IRC.Bot.PosixLogger where

import Commands
import Control.Concurrent.Chan
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (UTCTime(..), addUTCTime, getCurrentTime)
import Data.Time.Format (formatTime)
import Network.IRC (Command, Message(Message, msg_prefix, msg_command, msg_params), Prefix(NickName), UserName, encode, decode, joinChan, nick, user)
import System.FilePath
import System.Locale
import System.Log.Logger (Priority(DEBUG, INFO, ERROR), logM, rootLoggerName, setHandlers, setLevel, updateGlobalLogger)
import System.Posix

-- TODO: This should be modified so that a formatting filter can be applied to the log messages
-- TODO: should be updated so that log file name matches channel
-- TODO: should support multiple channels
logger :: Maybe FilePath -> String -> Chan Message -> IO ()
logger mLogDir channel logChan =
  do now <- getCurrentTime
     let logDay = utctDay now
     logFd <- openLog now
     logLoop logDay logFd
    where
      openLog :: UTCTime -> IO (Maybe Fd)
      openLog now =
          case mLogDir of
            Nothing -> return Nothing
            (Just logDir) ->
                do let logPath = logDir </> (formatTime defaultTimeLocale "happs-%Y-%m-%d.txt" now)
                   fd <- openFd logPath WriteOnly (Just 0o0644) (defaultFileFlags { append = True })
                   return (Just fd)
      updateLogHandle :: UTCTime -> Day -> Maybe Fd -> IO (Day, Maybe Fd)
      updateLogHandle now logDay Nothing = return (logDay, Nothing)
      updateLogHandle now logDay (Just logFd)
        | logDay == (utctDay now) = return (logDay, Just logFd)
        | otherwise = do closeFd logFd
                         nowHandle <- openLog now
                         return (utctDay now, nowHandle)
          
      logLoop :: Day -> Maybe Fd -> IO ()
      logLoop logDay mLogFd =
        do msg <- readChan logChan
           now <- getCurrentTime 
           (logDay', mLogFd') <- updateLogHandle now logDay mLogFd
           let mPrivMsg = toPrivMsg msg
           case mPrivMsg of
             (Just (PrivMsg (Just (NickName nick _user _server)) receivers msg)) | channel `elem` receivers ->
                   do let logMsg = showString (formatTime defaultTimeLocale "%X " now) . showString "<" . showString nick . showString "> " $ msg
                      logM "irc" INFO logMsg
                      case mLogFd' of
                        Nothing -> return ()
                        (Just logFd') -> fdWrite logFd' (logMsg ++ "\n") >> return ()
                      return ()
                      -- hPutStrLn logFd logMsg
             _ -> logM "irc" INFO (show msg)
           logLoop logDay' mLogFd'
