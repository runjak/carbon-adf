module OpenBrain.Data.TimeFrame(
    CalendarTime
  , TimeFrame(..)
)where

import System.Time (CalendarTime)

class TimeFrame t where
  creation :: t -> CalendarTime
  deletion :: t -> Maybe CalendarTime
