{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Datepicker where

import System.Time (CalendarTime(..))
import Text.Hastache
import Text.Hastache.Context
import qualified System.Time as Time

import OpenBrain.Website.Template

datepicker :: IO HTML
datepicker = do
  t <- Time.toCalendarTime =<< Time.getClockTime
  let date = show (ctYear t) ++ "-"++ (show . (+1) . fromEnum $ ctMonth t)
                             ++ "-" ++ show (ctDay t + 3)
      time = show (ctHour t) ++ ":" ++ prettify (ctMin t)
      context "Date" = MuVariable date
      context "Time" = MuVariable time
  tmpl "Datepicker.html" context
  where
    prettify = reverse . take 2 . reverse . ('0':) . show
