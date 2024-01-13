{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Draw (toPng) where

import Classify (EventType (..))
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import Text.Printf (printf)

pitem :: (EventType, Double) -> PieItem
pitem (eventType, hour) =
  pitem_value .~ hour $
    pitem_label .~ (eventType.typeName) ++ " " ++ printf "%.2f" hour $
      def

toPng :: FilePath -> [(EventType, Double)] -> IO ()
toPng path datas = toFile def path $ do
  let
    timePercent = map (\(t, v) -> (t, v / 3600)) datas
  pie_title .= "calendar"
  pie_plot . pie_data .= map pitem timePercent
