{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

import Classify (ClassifyConfig, EventType (..), classifyEvent)
import Cli (
  CalendarSummaryOption (..),
  parseCli,
 )
import Control.Monad (unless)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.Aeson qualified as A
import Data.Bifunctor (Bifunctor (second), bimap)
import Data.Functor ((<&>))
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Time (TimeZone, getCurrentTimeZone, localTimeToUTC)
import Draw (toPng)
import ParseVDirSyncer (
  filterAccordingToTime,
  parseEventsUsingCache,
 )
import Summarize (accountEvent, checkEvent)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Text.Show.Unicode (uprint, ushow)

-- newtype EventWithTimeCost = EventWithTimeCost {eventWithTimeCost :: (EventType, Double)}

mainFunc :: TimeZone -> Cli.CalendarSummaryOption -> ExceptT String (WriterT [String] IO) ()
mainFunc timeZone options = do
  let
    cacheDir = takeDirectory options.cacheJSONPath
  l2 $ createDirectoryIfMissing True cacheDir
  classifyConfig :: ClassifyConfig <- l2 $ A.decodeFileStrict options.classifyConfig <&> fromJust
  events <- parseEventsUsingCache options.cacheJSONPath options.calendarDir
  let
    eventsInRange = mapMaybe (filterAccordingToTime (bimap f f options.timeRange)) events
     where
      f = localTimeToUTC timeZone
    classfiedEvent = zip eventsInRange (map (either error id . classifyEvent classifyConfig) eventsInRange)
    unknownEvents = filter (\(_, t) -> null t) classfiedEvent
    checkEventRes = checkEvent timeZone eventsInRange
    statistics = accountEvent (map (second (fromMaybe (EventType "unknown"))) classfiedEvent)
    eventsWithTimeCost = M.toList statistics
  lift $ tell ["unknownEvents: " <> ushow unknownEvents]
  unless (null checkEventRes) $ lift $ tell [ushow checkEventRes]
  let
    totalTime = foldl (\a (_, b) -> a + b) 0.0 eventsWithTimeCost
  lift $ tell ["totalTime: " <> show totalTime]
  l2 $ toPng options.outputPng eventsWithTimeCost
  l2 $ mapM_ (\(EventType a, t) -> uprint (a, t / 3600.0)) eventsWithTimeCost
 where
  l2 = lift . lift

main :: IO ()
main = do
  options <- Cli.parseCli
  timeZone <- getCurrentTimeZone
  (runResult, warnings) <- runWriterT $ runExceptT $ mainFunc timeZone options
  putStrLn "warnings: "
  mapM_ (\x -> putStrLn $ "  " <> x) warnings
  case runResult of
    Left err -> putStrLn $ "error: " <> err
    Right () -> return ()
