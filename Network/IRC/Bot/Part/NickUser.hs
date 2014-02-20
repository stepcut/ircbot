module Network.IRC.Bot.Part.NickUser where

import Control.Monad.Trans        (liftIO)
import Data.ByteString            (ByteString)
import Data.ByteString.Char8      (pack, unpack)
import Data.Monoid                ((<>))
import Network.IRC.Bot.BotMonad   (BotMonad(..) )
import Network.IRC.Bot.Types      (User(..))
import Network.IRC.Bot.ErrorCodes
import Network.IRC.Bot.Log        (LogLevel(..))
-- import Network.IRC.Bot.Commands (Ping(..), Pong(..), ping, sendCommand)
import           Network.IRC      (Message(..), showMessage)
import qualified Network.IRC      as IRC
import System.Random              (randomRIO)

nickUserPart :: (BotMonad m) => m ()
nickUserPart =
  do msg <- askMessage
     let cmd = msg_command msg
     case () of
       () | cmd == noNicknameGiven  -> logM Important (showMessage msg)
          | cmd == erroneusNickname -> logM Important (showMessage msg)
          | cmd == nickCollision    -> logM Important (showMessage msg)
          | cmd == nicknameInUse    ->
              do logM Important (showMessage msg)
                 n <- whoami
                 i <- liftIO $ randomRIO (1, 100 :: Int)
                 changeNickUser (n <> pack (show i)) Nothing
          | otherwise -> return ()

changeNickUser :: (BotMonad m) => ByteString -> Maybe User -> m ()
changeNickUser n mUser =
    do sendMessage (IRC.nick n)
       case mUser of
         Nothing -> return ()
         (Just u) ->
             sendMessage (IRC.user (username u) (pack $ hostname u) (pack $ servername u) (realname u))
