{-# LANGUAGE DeriveDataTypeable #-}
module Network.IRC.Bot.Log where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Data

data LogLevel
    = Debug
    | Normal
    | Important
      deriving (Eq, Ord, Read, Show, Data, Typeable)

type Logger = LogLevel -> ByteString -> IO ()

stdoutLogger :: LogLevel -> Logger
stdoutLogger minLvl msgLvl msg
    | msgLvl >= minLvl = C.putStrLn msg -- assumes ascii, which is wrong(?)
    | otherwise        = return ()

nullLogger :: Logger
nullLogger _ _ = return ()