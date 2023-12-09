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
import Data.Time (TimeZone, ZonedTime (..), diffUTCTime, utcToZonedTime)
import Event (Event (..))

data ZonedEvent = ZonedEvent
  { summary :: String
  , start :: ZonedTime
  , end :: ZonedTime
  }
  deriving (Show)

eventToZonedEvent :: TimeZone -> Event -> ZonedEvent
eventToZonedEvent tz (Event su s e) = ZonedEvent su (utcToZonedTime tz s) (utcToZonedTime tz e)

data CheckError = NotEndToEnd (ZonedEvent, ZonedEvent) | EmptyEvent
  deriving
    (Show)

checkEndToEnd :: TimeZone -> [Event] -> Maybe CheckError
checkEndToEnd _ [] = Just EmptyEvent
checkEndToEnd _ [_] = Nothing
checkEndToEnd timeZone (x : y : xs) =
  if x.endTime /= y.startTime
    then Just (NotEndToEnd (eventToZonedEvent timeZone x, eventToZonedEvent timeZone y))
    else checkEndToEnd timeZone (y : xs)

checkEvent :: TimeZone -> [Event] -> Maybe CheckError
checkEvent tz evs = checkEndToEnd tz (sort evs)

accountEvent :: [(Event, EventType)] -> M.Map EventType Double
accountEvent =
  foldl
    (\mp (e, t) -> M.insertWith (+) t ((fromRational . toRational) (diffUTCTime e.endTime e.startTime)) mp)
    M.empty
