module OpenBrain.Data.Information where

import System.Time (CalendarTime)

import OpenBrain.Data.Id
import qualified OpenBrain.Data.User as User

type InformationId = Id
data Information = Information {
    author      :: User.UserData
  , creation    :: CalendarTime
  , description :: String
  , id          :: InformationId
  , name        :: String
  , media       :: Media
  , parent      :: Maybe Information
  , title       :: String
  }

data Media =
    Link        String
  | Text        String
  | Image       String
  | Video       String
  | Collection  [Information]
  | Discussion -- FIXME fill in
  | Decision -- FIXME fill in
