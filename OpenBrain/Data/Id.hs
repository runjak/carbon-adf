{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OpenBrain.Data.Id (
    Id, wrap, unwrap
  , IdType(..)
  , ArticleId, CollectionId, DescriptionId, NewDescriptionId, DiscussionId, RelationId, ResultId, UserId          
) where

import Data.Aeson (ToJSON, FromJSON)
import Happstack.Server as S

newtype Id = Id Integer
  deriving (Eq, Ord, Enum, Read, Show, ToJSON, FromJSON)

wrap :: Integer -> Id
wrap = Id

unwrap :: Id -> Integer
unwrap (Id i) = i

class IdType i where
  fromId  :: Id -> i
  toId    :: i -> Id

-- | Id Types:
newtype ArticleId        = AId   Id deriving (Eq, Ord, Enum, Read, Show, ToJSON, FromJSON)
newtype CollectionId     = CId   Id deriving (Eq, Ord, Enum, Read, Show, ToJSON, FromJSON)
newtype DescriptionId    = DeId  Id deriving (Eq, Ord, Enum, Read, Show, ToJSON, FromJSON)
newtype NewDescriptionId = NId   Id deriving (Eq, Ord, Enum, Read, Show, ToJSON, FromJSON)
newtype DiscussionId     = DiId  Id deriving (Eq, Ord, Enum, Read, Show, ToJSON, FromJSON)
newtype RelationId       = RelId Id deriving (Eq, Ord, Enum, Read, Show, ToJSON, FromJSON)
newtype ResultId         = ResId Id deriving (Eq, Ord, Enum, Read, Show, ToJSON, FromJSON)
newtype UserId           = UId   Id deriving (Eq, Ord, Enum, Read, Show, ToJSON, FromJSON)

-- | Instances for external classes:
instance FromReqURI Id where
  fromReqURI = return . wrap . read
