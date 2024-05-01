{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cli (
  CalendarSummaryOption (..),
  parseCli,
) where

import Classify (ClassifyConfig)
import Data.Aeson qualified as A
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Time (
  Day (..),
  LocalTime (..),
  TimeOfDay (TimeOfDay),
  defaultTimeLocale,
  getCurrentTime,
  getCurrentTimeZone,
  parseTimeM,
  utcToLocalTime,
 )
import GHC.Generics (Generic)
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
  optional,
  prefs,
  short,
  showHelpOnError,
  strOption,
  (<|>),
 )
import Options.Applicative.Types (ReadM)
import System.Directory (XdgDirectory (..), getXdgDirectory)

data CliTimeRange
  = CliTimeRangeDay Day
  | CliTimeRangeRange LocalTime (Maybe LocalTime)

data CliOption = CliOption
  { calendarDir :: Maybe FilePath
  , timeRange :: Maybe CliTimeRange
  , outputPng :: FilePath
  , cacheJSONPath :: Maybe FilePath
  , configPath :: Maybe FilePath
  }

-- 如果没输入，那么 timeRange 默认为当天
-- timeRange : 默认当天，或者 --date，或者 --startTime 到现在，或者指定 --startTime 到 --endTime
-- outputPng : 必要
-- cacheJSONPath : 有默认值
-- config : 必要，包含分类设置（必要）跟 calendarDir（可选）
-- calendarDir : 可选，取决于有没有
cliParser :: Parser CliOption
cliParser = do
  calendarDir <-
    optional $
      strOption
        ( long "calendar-dir"
            <> short 'c'
            <> help "calendar directory"
        )
  timeRange <- optional timeRangeParser
  outputPng :: FilePath <-
    strOption
      ( long "png"
          <> short 'p'
          <> help "output png file path"
          <> metavar "PNG_FILE"
      )
  cacheJSONPath <-
    optional $
      strOption
        ( long "cache"
            <> metavar "CACHE_JSON_PATH"
            <> help "cache json file path, default to $XDG_CACHE_HOME/calendar-visualization/cache.json"
        )
  configPath <-
    optional $
      strOption
        ( long "config"
            <> help "configuration file, default to $XDG_CONFIG_HOME/calendar-visualization/config.json"
            <> metavar "CONFIG"
        )
  pure
    CliOption
      { calendarDir = calendarDir
      , timeRange = timeRange
      , outputPng = outputPng
      , cacheJSONPath = cacheJSONPath
      , configPath = configPath
      }
 where
  dateReader = maybeReader $ parseTimeM False defaultTimeLocale "%Y-%m-%d" :: ReadM Day
  dateTimeReader = maybeReader $ parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S" :: ReadM LocalTime
  dateParser = option dateReader (long "date" <> short 'd' <> help "date")
  startTimeParser = option dateTimeReader (long "start-time" <> short 's' <> help "start time")
  endTimeParser = option dateTimeReader (long "end-time" <> short 'e' <> help "end time")

  timeRangeParser =
    CliTimeRangeDay
      <$> dateParser
        <|> CliTimeRangeRange
      <$> startTimeParser
      <*> optional endTimeParser

data ConfigFromFile = ConfigFromFile
  { calendarDir :: Maybe FilePath
  , classifyConfig :: ClassifyConfig
  }
  deriving (Show, Generic)

instance A.FromJSON ConfigFromFile
instance A.ToJSON ConfigFromFile

data CalendarSummaryOption = CalendarSummaryOption
  { calendarDir :: FilePath
  , timeRange :: (LocalTime, LocalTime)
  , outputPng :: FilePath
  , cacheJSONPath :: FilePath
  , classifyConfig :: ClassifyConfig
  }

parseCli :: IO CalendarSummaryOption
parseCli = do
  defaultCacheFile <- getXdgDirectory XdgCache "calendar-visualization/cache.json"
  defaultConfigFile <- getXdgDirectory XdgConfig "calendar-visualization/config.json"
  timeZone <- getCurrentTimeZone
  time <- getCurrentTime
  let
    currentDay :: Day = utcToLocalTime timeZone time & localDay
  cliOption <-
    customExecParser
      (prefs showHelpOnError)
      (info (helper <*> cliParser) fullDesc)
  let
    configFile = fromMaybe defaultConfigFile cliOption.configPath
  config :: ConfigFromFile <- A.eitherDecodeFileStrict configFile <&> either error id
  let
    calendarDir = case cliOption.calendarDir of
      Just x -> x
      Nothing -> case config.calendarDir of
        Just x -> x
        Nothing -> error "calendarDir is not specified either in command line or in config file"
  return
    CalendarSummaryOption
      { calendarDir = calendarDir
      , timeRange = case cliOption.timeRange of
          Just (CliTimeRangeDay day) ->
            ( LocalTime{localDay = day, localTimeOfDay = TimeOfDay 0 0 0}
            , LocalTime{localDay = succ day, localTimeOfDay = TimeOfDay 0 0 0}
            )
          Just (CliTimeRangeRange startTime maybeEndTime) -> case maybeEndTime of
            Just endTime -> (startTime, endTime)
            Nothing -> (startTime, LocalTime{localDay = succ (localDay startTime), localTimeOfDay = TimeOfDay 0 0 0})
          Nothing ->
            ( LocalTime{localDay = currentDay, localTimeOfDay = TimeOfDay 0 0 0}
            , LocalTime{localDay = succ currentDay, localTimeOfDay = TimeOfDay 0 0 0}
            )
      , outputPng = cliOption.outputPng
      , cacheJSONPath = fromMaybe defaultCacheFile cliOption.cacheJSONPath
      , classifyConfig = config.classifyConfig
      }
