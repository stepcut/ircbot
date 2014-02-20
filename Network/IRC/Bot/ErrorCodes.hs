{-# LANGUAGE OverloadedStrings #-}
module Network.IRC.Bot.ErrorCodes where

import Data.ByteString (ByteString)

-- * Nickname errors

noNicknameGiven :: ByteString
noNicknameGiven = "431"

erroneusNickname :: ByteString
erroneusNickname = "432"

nicknameInUse :: ByteString
nicknameInUse = "433"

nickCollision :: ByteString
nickCollision = "436"

