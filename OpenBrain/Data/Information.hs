{-# LANGUAGE TypeSynonymInstances #-}
module OpenBrain.Data.Information where

import System.Time (CalendarTime)

import OpenBrain.Data.Id
import qualified OpenBrain.Data.User as User

type InformationId = Id
data Information = Information {
    author        :: User.UserData
  , creation      :: CalendarTime
  , description   :: String
  , informationId :: InformationId
  , media         :: Media
  , title         :: String
  } deriving (Eq, Show)

data Media =
    Link        String
  | Text        String
  | Image       String
  | Video       String
  | Collection  [Information]
  | Discussion {
    arguments     :: [Information]          -- | Arguments that make up the Discussion
  , afType        :: ArgumentationFramework -- | Choosen on creation
  , choices       :: [([Information], Int)] -- | Groups of Informations with their vote count
  , complete      :: Maybe Media            -- | Is expected to be a Decision
  , deadline      :: CalendarTime           -- | After this no changes to arguments are allowed any longer
  , participants  :: [User.UserData]        -- | Anyone that decided to be a participant before the deadline
  , voted         :: [User.UserData]        -- | Users that already voted and are no longer allowed
  }
  | Decision {
    result :: [Information]
  } deriving (Eq, Show)

data ArgumentationFramework = AttackOnly | AttackDefense deriving (Show, Read, Eq, Enum)

class InformationIdentifier i where
  getInformationId :: i -> InformationId

instance InformationIdentifier InformationId where
  getInformationId = id

instance InformationIdentifier Information where
  getInformationId = informationId

