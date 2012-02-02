{-# LANGUAGE DeriveDataTypeable #-}
module Network.IRC.Bot.Commands where

import Control.Applicative
import Control.Monad
import Data.Data
import Network (HostName, PortID(PortNumber))
import Network.IRC
import Network.IRC.Bot.BotMonad

-- * Commands
     
cmd :: (Functor m, MonadPlus m, BotMonad m) => Command -> m ()
cmd cmdName =
  do command <- msg_command <$> askMessage
     if cmdName == command
       then return ()
       else mzero

data Ping
  = Ping HostName
  deriving (Eq, Ord, Read, Show, Data, Typeable)
           
ping :: (Functor m, MonadPlus m, BotMonad m) => m Ping
ping =            
  do cmd "PING"
     params <- msg_params  <$> askMessage
     case params of
       (hostName:_) -> return $ Ping hostName
       _ -> mzero
       
           
data PrivMsg           
  = PrivMsg { prefix     :: (Maybe Prefix) 
            , receivers  :: [String]
            , msg        :: String
            }
      deriving (Eq, Read, Show)
       
privMsg :: (Functor m, MonadPlus m, BotMonad m) => m PrivMsg
privMsg =
  do msg <- askMessage
     maybe mzero return (toPrivMsg msg)
     
toPrivMsg :: Message -> Maybe PrivMsg     
toPrivMsg msg =
  let cmd    = msg_command msg
      params = msg_params  msg
      prefix = msg_prefix  msg
  in case cmd of
      "PRIVMSG" -> Just $ PrivMsg prefix (init params) (last params)
      _         -> Nothing
     
class ToMessage a where
  toMessage :: a -> Message
  
sendCommand :: (ToMessage c, BotMonad m, Functor m) => c -> m ()
sendCommand c = sendMessage (toMessage c)

data Pong
  = Pong HostName
      deriving (Eq, Ord, Read, Show, Data, Typeable)

instance ToMessage Pong where
    toMessage (Pong hostName) = Message Nothing "PONG" [hostName]

instance ToMessage PrivMsg where
    toMessage (PrivMsg prefix receivers msg) = Message prefix "PRIVMSG" (receivers ++ [msg])

