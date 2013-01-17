module OpenBrain.Data.Misc where
{-
  Types used in OpenBrain.Backend
-}

import OpenBrain.Data.Described as Described
import OpenBrain.Data.Id
import OpenBrain.Data.Information (CollectionType(..))
import OpenBrain.Data.TimeFrame as TimeFrame

type Limit  = Int
type Offset = Int
type Count  = Int

data CreateInformation = CreateInformation {
    userId        :: UserId
  , ciTitle       :: Title
  , ciDescription :: Description
} deriving (Show)
instance Described CreateInformation where
  title       = ciTitle
  description = ciDescription

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
