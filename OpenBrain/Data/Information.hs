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

type Voted = Bool
data Media =
    Media String -- | HTML rich content
  | Collection [Information]
  | Discussion {
    arguments     :: [Information]            -- | Arguments that make up the Discussion
  , afType        :: ArgumentationFramework   -- | Choosen on creation
  , choices       :: [([Information], Int)]   -- | Groups of Informations with their vote count
  , complete      :: Maybe Information        -- | Media of that Information should always be a Decision
  , deadline      :: CalendarTime             -- | After this no changes to arguments are allowed any longer
  , participants  :: [(User.UserData, Voted)] -- | Anyone that decided to be a participant before the deadline
  }
  | Decision [Information]
  deriving (Eq, Show)

data ArgumentationFramework = AttackOnly | AttackDefense deriving (Show, Read, Eq, Enum)

class InformationIdentifier i where
  getInformationId :: i -> InformationId

instance InformationIdentifier InformationId where
  getInformationId = id

instance InformationIdentifier Information where
  getInformationId = informationId

