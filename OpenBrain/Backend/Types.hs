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

type Content = String

data DiscussionType = AttackOnly | AttackDefense
toCollectionType :: DiscussionType -> CollectionType
toCollectionType AttackOnly     = DiscussionAttackOnly
toCollectionType AttackDefense  = DiscussionAttackDefense

type Deadline = CalendarTime

type Target = InformationId
type Source = InformationId

type Comment = String
type ActionKey = String
