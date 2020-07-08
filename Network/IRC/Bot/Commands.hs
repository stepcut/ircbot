{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Network.IRC.Bot.Commands where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Data
import Data.Monoid ((<>))
import Network.IRC (Message(Message, msg_prefix, msg_command, msg_params), Command, Prefix(NickName))
import Network.IRC.Bot.BotMonad (BotMonad, askMessage, sendMessage)

type HostName = ByteString

-- * Commands

cmd :: (Functor m, MonadPlus m, BotMonad m) => Command -> m ()
cmd cmdName =
  do command' <- msg_command <$> askMessage
     if cmdName == command'
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
            , receivers  :: [ByteString]
            , msg        :: ByteString
            }
      deriving (Eq, Read, Show)

privMsg :: (Functor m, MonadPlus m, BotMonad m) => m PrivMsg
privMsg =
  do msg' <- askMessage
     maybe mzero return (toPrivMsg msg')

toPrivMsg :: Message -> Maybe PrivMsg
toPrivMsg m =
  let cmd'    = msg_command m
      params  = msg_params  m
      prefix' = msg_prefix  m
  in case cmd' of
      "PRIVMSG" -> Just $ PrivMsg prefix' (init params) (last params)
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
    toMessage (PrivMsg prefix' receivers' msg') = Message prefix' "PRIVMSG" (receivers' <> [msg'])

data Notice
  = Notice  { noticePrefix     :: (Maybe Prefix)
            , noticeReceivers  :: [ByteString]
            , noticeMsg        :: ByteString
            }
      deriving (Eq, Read, Show)

instance ToMessage Notice where
    toMessage (Notice prefix' receivers' msg') = Message prefix' "NOTICE" (receivers' <> [msg'])

-- | get the nickname of the user who sent the message
askSenderNickName :: (BotMonad m) => m (Maybe ByteString)
askSenderNickName =
    do msg' <- askMessage
       case msg_prefix msg' of
         (Just (NickName nick _ _)) -> return (Just nick)
         _ -> return Nothing

-- | figure out who to reply to for a given `Message`
--
-- If message was sent to a #channel reply to the channel. Otherwise reply to the sender.
replyTo :: (BotMonad m) => m (Maybe ByteString)
replyTo =
    do priv <- privMsg
       let receiver = head (receivers priv)
       if ("#" `B.isPrefixOf` receiver)
          then return (Just receiver)
          else askSenderNickName

-- | returns the receiver of a message
--
-- if multiple receivers, it returns only the first
askReceiver :: (Alternative m, BotMonad m) => m (Maybe ByteString)
askReceiver =
    do priv <- privMsg
       return (Just (head $ receivers priv))
    <|>
    do return Nothing

