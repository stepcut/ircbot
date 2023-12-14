{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, RankNTypes, RecordWildCards, OverloadedStrings #-}
module Main where

import Network.IRC.Bot            (runBotWithParts)
import Network.IRC.Bot.Part.Dice  (dicePart)
import Network.IRC.Bot.Part.Hello (helloPart)
import Network.IRC.Bot.Part.NickUser (nickUserPart)

main :: IO ()
main = runBotWithParts $ pure
  [ nickUserPart
  , dicePart
  , helloPart
  ]
