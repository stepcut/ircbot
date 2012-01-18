{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Network.IRC.Bot.Types
    ( User(..)
    , nullUser
    ) where

import Control.Concurrent       (ThreadId, forkIO, threadDelay)
import Control.Concurrent.Chan  (Chan, dupChan, newChan, readChan, writeChan)
import Control.Concurrent.MVar  (MVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.QSem  (QSem, newQSem, waitQSem, signalQSem)
import Control.Exception        (IOException, catch)
import Control.Monad            (mplus, forever, when)
import Control.Monad.Trans      (liftIO)
import Data.Data                (Data, Typeable)
import Data.Time                (UTCTime, addUTCTime, getCurrentTime)
import Network                  (HostName, PortID(PortNumber), connectTo)
import Network.IRC              (Message, decode, encode, joinChan, nick, user)
import Network.IRC              as I
import Network.IRC.Bot.Log      (Logger, LogLevel(Normal, Debug), stdoutLogger)
import Network.IRC.Bot.BotMonad (BotMonad(logM), BotPartT, BotEnv(..), runBotPartT)
import Prelude                  hiding (catch)
import System.IO                (BufferMode(LineBuffering), Handle, hClose, hGetLine, hPutStrLn, hSetBuffering)


data User = User
    { username   :: String    -- ^ username on client system
    , hostname   :: HostName  -- ^ hostname of client system
    , servername :: HostName  -- ^ irc server client is connected to
    , realname   :: String    -- ^ client's real name
    }
    deriving (Data, Typeable, Eq, Ord, Read, Show)

nullUser :: User
nullUser = User { username   = ""
                , hostname   = "."
                , servername = "."
                , realname   = ""
                }
