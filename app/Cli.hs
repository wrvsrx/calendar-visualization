{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cli (
  CalendarSummaryOption (..),
  parseCli,
) where

import Data.Time (
  Day (..),
  LocalTime (..),
  TimeOfDay (TimeOfDay),
  defaultTimeLocale,
  parseTimeM,
 )
import Options.Applicative (
  Parser,
  customExecParser,
  fullDesc,
  help,
  helper,
  info,
  long,
  maybeReader,
  metavar,
  option,
  prefs,
  short,
  showDefault,
  showHelpOnError,
  strOption,
  value,
  (<|>),
 )
import Options.Applicative.Types (ReadM)
import System.Directory (XdgDirectory (..), getXdgDirectory)

data CalendarSummaryOption = CalendarSummaryOption
  { calendarDir :: FilePath
  , timeRange :: (LocalTime, LocalTime)
  , outputPng :: FilePath
  , cacheJSONPath :: FilePath
  , classifyConfig :: FilePath
  }

optionParser :: FilePath -> FilePath -> Parser CalendarSummaryOption
optionParser defaultCacheFile defaultConfigFile =
  CalendarSummaryOption
    <$> strOption
      ( long "calendar-dir"
          <> short 'c'
          <> help "calendar directory"
      )
    <*> (dateToTimeRangeParser <|> rangeToRangeParser)
    <*> strOption
      ( long "png"
          <> short 'p'
          <> help "output png file path"
          <> metavar "PNG_FILE"
      )
    <*> strOption
      ( long "cache"
          <> showDefault
          <> value defaultCacheFile
          <> metavar "CACHE_JSON_PATH"
          <> help "cache json file path"
      )
    <*> strOption
      ( long "classify"
          <> help "classify configuration"
          <> metavar "CONFIG"
          <> showDefault
          <> value defaultConfigFile
      )
 where
  dateReader = maybeReader $ parseTimeM False defaultTimeLocale "%Y-%m-%d" :: ReadM Day
  dateTimeReader = maybeReader $ parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S" :: ReadM LocalTime
  dateToTimeRangeParser = do
    date <- option dateReader (long "date" <> short 'd' <> help "date")
    pure
      ( LocalTime{localDay = date, localTimeOfDay = TimeOfDay 0 0 0}
      , LocalTime{localDay = succ date, localTimeOfDay = TimeOfDay 0 0 0}
      )
  rangeToRangeParser = do
    startTime <- option dateTimeReader (long "start-time" <> short 's' <> help "start time")
    endTime <- option dateTimeReader (long "end-time" <> short 'e' <> help "end time")
    pure (startTime, endTime)

parseCli :: IO CalendarSummaryOption
parseCli = do
  defaultCacheFile <- getXdgDirectory XdgCache "calendar-visualization/cache.json"
  defaultConfigFile <- getXdgDirectory XdgConfig "calendar-visualization/classifyConfig.json"
  customExecParser
    (prefs showHelpOnError)
    (info (helper <*> optionParser defaultCacheFile defaultConfigFile) fullDesc)
