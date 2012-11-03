module OpenBrain.Backend.Types where
{-
  Types used in OpenBrain.Backend
-}

import System.Time (CalendarTime)

import OpenBrain.Data.Id
import OpenBrain.Data.Information (CollectionType(..))

type Limit  = Int
type Offset = Int
type Count  = Int

type Title = String
type Description = String

data CreateInformation = CreateInformation {
    userId      :: UserId
  , title       :: Title
  , description :: Description
}

type Heir = UserId

type Content = String

data DiscussionType = AttackOnly | AttackDefense deriving (Show, Read)

toCollectionType :: DiscussionType -> CollectionType
toCollectionType AttackOnly     = DiscussionAttackOnly
toCollectionType AttackDefense  = DiscussionAttackDefense

isDiscussionType :: CollectionType -> Bool
isDiscussionType DiscussionAttackOnly     = True
isDiscussionType DiscussionAttackDefense  = True
isDiscussionType _                        = False

type Deadline = CalendarTime

type Collection = InformationId

type Target       = InformationId
type Source       = InformationId
data RelationEnd  = RelationSource
                  | RelationTarget
type AllowDeleted = Bool
type Comment      = String

type ActionKey = String
