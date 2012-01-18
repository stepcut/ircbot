module Network.IRC.Bot.ErrorCodes where

-- * Nickname errors

noNicknameGiven :: String
noNicknameGiven = "431"

erroneusNickname :: String
erroneusNickname = "432"

nicknameInUse :: String
nicknameInUse = "433"

nickCollision :: String
nickCollision = "436"

