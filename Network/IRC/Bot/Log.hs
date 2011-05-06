{-# LANGUAGE DeriveDataTypeable #-}
module Network.IRC.Bot.Log where

import Data.Data

data LogLevel
    = Debug
    | Normal
    | Important
      deriving (Eq, Ord, Read, Show, Data, Typeable)

type Logger = LogLevel -> String -> IO ()

stdoutLogger :: LogLevel -> Logger
stdoutLogger minLvl msgLvl msg
    | msgLvl >= minLvl = putStrLn msg
    | otherwise        = return ()

nullLogger :: Logger
nullLogger _ _ = return ()