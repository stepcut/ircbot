{- |
Module      :  Network.IRC.Bot.Limiter
Description :  simple rate limiter
Copyright   :  (c) 2012 Eric Mertens
License     :  BSD3

Maintainer  :  jeremy@seereason.com
Stability   :  stable
Portability :  portable

A simple rate limiter.
-}
module Network.IRC.Bot.Limiter
    ( Limiter(..)
    , newLimiter
    , limit
    )
    where

import Control.Concurrent      (ThreadId, forkIO, threadDelay)
import           Control.Concurrent.SSem (SSem)
import qualified Control.Concurrent.SSem as SSem
import Control.Monad           (forever)

data Limiter = Limiter
    { limitsIn       :: SSem
    , limitsOut      :: SSem
    , limitsDelay    :: Int
    , limitsThreadId :: ThreadId
    }

-- | Construct a new rate limit control
newLimiter :: Int -- ^ max burst length
           -> Int -- ^ delay (in microseconds)
           -> IO Limiter
newLimiter burst delay = do
  rdy  <- SSem.new burst
  sent <- SSem.new 0
  let l = Limiter { limitsIn       = sent
                  , limitsOut      = rdy
                  , limitsDelay    = delay
                  , limitsThreadId = error "limiter thread not started yet"
                  }
  tid <- forkIO (limiter l)
  return $ l { limitsThreadId = tid }

-- | Execute this before sending
limit :: Limiter -> IO ()
limit l = do
  SSem.wait   (limitsOut l)
  SSem.signal (limitsIn l)

-- | Loop which manages the limit timers
limiter :: Limiter -> IO b
limiter l = forever $ do
  SSem.wait    (limitsIn l)
  threadDelay  (limitsDelay l)
  SSem.signal  (limitsOut l)