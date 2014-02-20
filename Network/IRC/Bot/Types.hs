{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Network.IRC.Bot.Types
    ( User(..)
    , nullUser
    ) where

import Data.ByteString          (ByteString)
import Data.Data                (Data, Typeable)
import Network.IRC              as I
import Network                  (HostName)

data User = User
    { username   :: ByteString    -- ^ username on client system
    , hostname   :: HostName  -- ^ hostname of client system
    , servername :: HostName  -- ^ irc server client is connected to
    , realname   :: ByteString    -- ^ client's real name
    }
    deriving (Data, Typeable, Eq, Ord, Read, Show)

nullUser :: User
nullUser = User { username   = ""
                , hostname   = "."
                , servername = "."
                , realname   = ""
                }
