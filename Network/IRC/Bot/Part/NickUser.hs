module Network.IRC.Bot.Part.NickUser where

import Control.Monad.Trans        (liftIO)
import Network.IRC.Bot.BotMonad   (BotMonad(..) )
import Network.IRC.Bot.Types      (User(..))
import Network.IRC.Bot.ErrorCodes 
import Network.IRC.Bot.Log        (LogLevel(..))
-- import Network.IRC.Bot.Commands (Ping(..), Pong(..), ping, sendCommand)
import           Network.IRC      (Message(..))
import qualified Network.IRC      as IRC
import System.Random              (randomRIO)

nickUserPart :: (BotMonad m) => m ()     
nickUserPart =
  do msg <- askMessage 
     let cmd = msg_command msg
     case () of
       () | cmd == noNicknameGiven  -> logM Important (show msg)
          | cmd == erroneusNickname -> logM Important (show msg)
          | cmd == nickCollision    -> logM Important (show msg)
          | cmd == nicknameInUse    -> 
              do logM Important (show msg)
                 n <- whoami
                 i <- liftIO $ randomRIO (1, 100 :: Int)
                 changeNickUser (n ++ show i) Nothing
          | otherwise -> return ()

changeNickUser :: (BotMonad m) => String -> Maybe User -> m ()
changeNickUser n mUser =
    do sendMessage (IRC.nick n)
       case mUser of
         Nothing -> return ()
         (Just u) ->
             sendMessage (IRC.user (username u) (hostname u) (servername u) (realname u))
