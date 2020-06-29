{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, RankNTypes, RecordWildCards, OverloadedStrings #-}
module Main where

import Network.IRC.Bot            (runBotWithParts)
import Network.IRC.Bot.Part.Dice  (dicePart)
import Network.IRC.Bot.Part.Hello (helloPart)
import Network.IRC.Bot.Part.NickUser (nickUserPart)
import Network.IRC.Bot.Part.Channels (initChannelsPart)

main :: IO ()
main = runBotWithParts $ pure
  [ nickUserPart
  , dicePart
  , helloPart
  ]
