module Network.IRC.Bot.Part.Ping where

import Network.IRC.Bot.BotMonad (BotMonad)
import Network.IRC.Bot.Commands (Ping(..), Pong(..), ping, sendCommand)

pingPart :: (BotMonad m) => m ()     
pingPart =
  do (Ping hostName) <- ping
     sendCommand (Pong hostName)
