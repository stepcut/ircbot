{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, UndecidableInstances #-}
module Network.IRC.Bot.BotMonad
    ( BotPartT(..)
    , BotMonad(..)
    , BotEnv(..)
    , runBotPartT
    , mapBotPartT
    , maybeZero
    ) where

import Control.Applicative (Applicative, Alternative, (<$>))
import Control.Arrow (first)
import Control.Monad (MonadPlus(mplus, mzero), forever, replicateM, when)
import Control.Monad.Cont   (MonadCont)
import Control.Monad.Error  (MonadError)
import Control.Monad.Reader (MonadReader(ask, local), MonadTrans, ReaderT(runReaderT), mapReaderT)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.State  (MonadState)
import Control.Monad.RWS    (MonadRWS)
import Control.Concurrent.Chan (Chan, dupChan, newChan, readChan, writeChan)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans
import Network.IRC (Command, Message(Message, msg_prefix, msg_command, msg_params), Prefix(NickName), UserName, encode, decode, joinChan, nick, user)
import Network.IRC.Bot.Log

class (Functor m, MonadPlus m, MonadIO m) => BotMonad m where
  askBotEnv    :: m BotEnv
  askMessage   :: m Message
  askOutChan   :: m (Chan Message)
  localMessage :: (Message -> Message) -> m a -> m a
  sendMessage  :: Message -> m ()
  logM         :: LogLevel -> String -> m ()
  whoami       :: m String

data BotEnv = BotEnv
    { message   :: Message
    , outChan   :: Chan Message
    , logFn     :: Logger
    , botName   :: String
    , cmdPrefix :: String
    }

newtype BotPartT m a = BotPartT { unBotPartT :: ReaderT BotEnv m a }
    deriving (Applicative, Alternative, Functor, Monad, MonadFix, MonadPlus, MonadTrans, MonadIO, MonadWriter w, MonadState s, MonadError e, MonadCont)

instance (MonadReader r m) => MonadReader r (BotPartT m) where
    ask     = BotPartT (lift ask)
    local f = BotPartT . mapReaderT (local f) . unBotPartT

instance (MonadRWS r w s m) => MonadRWS r w s (BotPartT m)

runBotPartT :: BotPartT m a -> BotEnv -> m a
runBotPartT botPartT = runReaderT (unBotPartT botPartT)

mapBotPartT :: (m a -> n b) -> BotPartT m a -> BotPartT n b
mapBotPartT f (BotPartT r) = BotPartT $ mapReaderT f r

instance (Functor m, MonadIO m, MonadPlus m) => BotMonad (BotPartT m) where
  askBotEnv  = BotPartT ask
  askMessage = BotPartT (message <$> ask)
  askOutChan = BotPartT (outChan <$> ask)
  localMessage f (BotPartT r) = BotPartT (local (\e -> e { message = f (message e) }) r)
  sendMessage msg =
    BotPartT $ do out <- outChan <$> ask
                  liftIO $ writeChan out msg
                  return ()
  logM lvl msg =
    BotPartT $ do l <- logFn <$> ask
                  liftIO $ l lvl msg
  whoami       =  BotPartT $ botName <$> ask

maybeZero :: (MonadPlus m) => Maybe a -> m a
maybeZero Nothing = mzero
maybeZero (Just a) = return a