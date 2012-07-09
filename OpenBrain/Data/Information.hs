module OpenBrain.Data.Information where

import Control.Monad.Trans.Maybe
import System.Time (CalendarTime)

import OpenBrain.Data.Id
import qualified OpenBrain.Data.User as User

type InformationId = Id
data Information = Information {
    author      :: User.UserData
  , creation    :: CalendarTime
  , description :: String
  , id          :: InformationId
  , media       :: Media
  , parent      :: MaybeT IO Information -- Function fetching the parent.
  , title       :: String
  }

data Media =
    Link        String
  | Text        String
  | Image       String
  | Video       String
  | Collection  [Information]
  | Discussion {
    participants  :: [User.UserData]
  , arguments     :: [Information]
  , afType        :: ArgumentationFramework
  }
  | Decision {
    -- result :: [Information]
    -- FIXME define
  }

data ArgumentationFramework = AF1 | AF2 -- Types of Argumentation Frameworks

