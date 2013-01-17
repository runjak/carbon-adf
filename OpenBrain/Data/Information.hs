module OpenBrain.Data.Information where

import Data.Function (on)

import OpenBrain.Data.Described
import OpenBrain.Data.Id
import OpenBrain.Data.TimeFrame
import qualified OpenBrain.Data.User as User

data Information = Information {
    author              :: User.UserData      -- | Every information has an author.
  , informationCreation :: CalendarTime       -- | The time of creation for the information.
  , informationDeletion :: Maybe CalendarTime -- | The time of deletion for the information
  , iDescription        :: String             -- | A short description of what the information is about.
  , informationId       :: InformationId
  , media               :: Media              -- | The real content
  , iTitle              :: String             -- | A title for the information
  } deriving (Show)
instance Described Information where
  title       = iTitle
  description = iDescription
instance Eq Information where
  (==) = (==) `on` informationId
instance Ord Information where
  compare = compare `on` informationId
instance TimeFrame Information where
  creation = informationCreation
  deletion = informationDeletion

data Media =
    Content String -- | HTML rich content
  | ICollection {  -- | Everything that is not a single information
    arguments       :: [InformationId]      -- | Informations that are grouped by this Collection.
  , collectionType  :: CollectionType       -- | The nature of this Collection.
  , discussion      :: Maybe DiscussionInfo -- | Additional Information if the Collection is a Discussiontype.
  }
instance Show Media where
  show (Content s)                = "Content " ++ show s
  show (ICollection args ctype d)  = "Collection {"
                                 ++ "arguments = [" ++ replicate (length args) '.' ++ "]"
                                 ++ ", collectionType = " ++ show ctype
                                 ++ ", discussion = " ++ show d
                                 ++ "}"

data CollectionType =       -- | The different kinds of collections of informations in OpenBrain
    SimpleCollection        -- | Information that is grouped by a user
  | Choice                  -- | A possible result of a discussion
  | Decision                -- | The winning choice of a discussion
  | DiscussionAttackOnly    -- | A discussion in which only attacks count.
  | DiscussionAttackDefense -- | A discussion that considers attacks and defenses.
  deriving (Show, Read, Eq, Enum)

type Votes = Int
type Voted = Bool
data DiscussionInfo = DiscussionInfo {        -- | If a collection is a discussion it should have discussioninfo with it.
    choices       :: [(Information, Votes)]   -- | A list of possible choices along with the votes they get.
  , complete      :: Maybe Information        -- | Target information should be a Decision.
  , deadline      :: CalendarTime             -- | When the Discussion doesn't allow for changing arguments and votes are cast.
  , participants  :: [(User.UserData, Voted)] -- | Users that take part in the discussion and if they already voted.
  } deriving (Eq, Show)

isContent :: Media -> Bool
isContent (Content _) = True
isContent _           = False

getContent :: Media -> String
getContent (Content c)  = c
getContent _            = error "No content in OpenBrain.Data.Information:getContent"

isDiscussion :: Media -> Bool
isDiscussion (ICollection _ _ (Just d)) = True
isDiscussion _                         = False

