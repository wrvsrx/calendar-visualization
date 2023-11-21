{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Classify (
  EventType (..),
  classifyEvent,
  ClassifyConfig (..),
) where

import Data.Aeson ((.:))
import Data.Aeson qualified as A
import Data.List.Split (splitOn)
import Data.Map qualified as M
import Data.Text qualified as T
import Event (Event (..))
import GHC.Generics (Generic)

newtype EventType = EventType {typeName :: String} deriving (Generic, Eq, Ord, Show)

instance A.FromJSON EventType where
  parseJSON a = case a of
    A.Object o -> EventType <$> (o .: "typeName")
    A.String s -> return (EventType (T.unpack s))
    _ -> fail "fail to parse"
instance A.ToJSON EventType

data ClassifyConfig = ClassifyConfig
  { eventTypes :: [EventType]
  , eventToType :: M.Map String EventType
  , eventAbbrToType :: M.Map String EventType
  }
  deriving (Generic)

instance A.FromJSON ClassifyConfig
instance A.ToJSON ClassifyConfig

classifyEvent :: ClassifyConfig -> Event -> Either String (Maybe EventType)
classifyEvent config (Event summary _ _) =
  -- summary pattern
  -- '<category>: <description>'
  -- '<summary>'
  let
    splitResult = splitOn ":" summary
    et = case splitResult of
      [eventTypeAbbr, description] -> do
        case description of
          ' ' : _ -> case M.lookup eventTypeAbbr config.eventAbbrToType of
            Just x -> Right (Just x)
            Nothing -> Left ("can't find such type: " <> eventTypeAbbr)
          _ -> Left "no space ahead of name"
      [summary_] ->
        let
          maybeEventType = M.lookup summary_ config.eventToType
         in
          case maybeEventType of
            Just et_ -> Right (Just et_)
            Nothing -> Right Nothing
      [] -> Left "impossible case"
      _ -> Left "too much `:`"
   in
    et
