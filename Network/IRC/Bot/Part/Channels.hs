module Network.IRC.Bot.Part.Channels where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Set (Set, insert, toList)
import Network.IRC (Message(..), joinChan)
import Network.IRC.Bot.BotMonad (BotMonad(..))
import Network.IRC.Bot.Log (LogLevel(..))

initChannelsPart :: (BotMonad m) => Set String -> IO (TVar (Set String), m ())
initChannelsPart chans =
    do channels <- atomically $ newTVar chans
       return (channels, channelsPart channels)

channelsPart :: (BotMonad m) => TVar (Set String) -> m ()
channelsPart channels =
    do msg <- askMessage
       let cmd = msg_command msg
       case cmd of
         "005" -> do chans <- liftIO $ atomically $ readTVar channels
                     mapM_ doJoin (toList chans)
         _ -> return ()
    where
      doJoin :: (BotMonad m) => String -> m ()
      doJoin chan = 
          do sendMessage (joinChan chan)
             logM Normal $ "Joining room " ++ chan

joinChannel :: (BotMonad m) => String -> TVar (Set String) -> m ()
joinChannel chan channels =
    do liftIO $ atomically $
           do cs <- readTVar channels
              writeTVar channels (insert chan cs)
       sendMessage (joinChan chan)
              