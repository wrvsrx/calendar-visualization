{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Summarize (
  checkEvent,
  accountEvent,
  CheckError (..),
) where

import Classify (EventType (..))
import Data.List (sort)
import Data.Map qualified as M
import Data.Time (diffUTCTime)
import Event (Event (..))

data CheckError = NotEndToEnd (Event, Event) | EmptyEvent
  deriving
    (Show)

checkEndToEnd :: [Event] -> Maybe CheckError
checkEndToEnd [] = Just EmptyEvent
checkEndToEnd [_] = Nothing
checkEndToEnd (x : y : xs) =
  if x.endTime /= y.startTime
    then Just (NotEndToEnd (x, y))
    else checkEndToEnd (y : xs)

checkEvent :: [Event] -> Maybe CheckError
checkEvent evs = checkEndToEnd (sort evs)

accountEvent :: [(Event, EventType)] -> M.Map EventType Double
accountEvent =
  foldl
    (\mp (e, t) -> M.insertWith (+) t ((fromRational . toRational) (diffUTCTime e.endTime e.startTime)) mp)
    M.empty
