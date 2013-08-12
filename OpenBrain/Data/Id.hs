{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module OpenBrain.Data.Id (
    Id, wrap, unwrap
  , IdType(..)
  , ArticleId, CollectionId, NewCollectionId, DescriptionId, NewDescriptionId, DiscussionId, RelationId, ResultId, UserId          
) where

import Control.Arrow (first)
import Control.Monad (liftM)
import Data.Aeson (ToJSON, FromJSON)
import Happstack.Server as S

newtype Id = Id Integer
  deriving (Eq, Ord, Enum, ToJSON, FromJSON)

wrap :: Integer -> Id
wrap = Id

unwrap :: Id -> Integer
unwrap (Id i) = i

class IdType i where
  fromId  :: Id -> i
  toId    :: i -> Id

-- | Id Types:
newtype ArticleId        = AId   Id deriving (Eq, Ord, Enum, ToJSON, FromJSON)
newtype CollectionId     = CId   Id deriving (Eq, Ord, Enum, ToJSON, FromJSON)
newtype NewCollectionId  = NCId  Id deriving (Eq, Ord, Enum, ToJSON, FromJSON)
newtype DescriptionId    = DeId  Id deriving (Eq, Ord, Enum, ToJSON, FromJSON)
newtype NewDescriptionId = NDId  Id deriving (Eq, Ord, Enum, ToJSON, FromJSON)
newtype DiscussionId     = DiId  Id deriving (Eq, Ord, Enum, ToJSON, FromJSON)
newtype RelationId       = RelId Id deriving (Eq, Ord, Enum, ToJSON, FromJSON)
newtype ResultId         = ResId Id deriving (Eq, Ord, Enum, ToJSON, FromJSON)
newtype UserId           = UId   Id deriving (Eq, Ord, Enum, ToJSON, FromJSON)

-- | Instances:
instance FromReqURI Id where
  fromReqURI = return . wrap . read

instance ToMessage Id where
  toContentType = const "application/json"
  toMessage     = toMessage . show . unwrap
  toResponse    = toResponse . show . unwrap

instance IdType ArticleId where
  fromId = AId
  toId (AId i) = i
instance IdType CollectionId where
  fromId = CId
  toId (CId i) = i
instance IdType NewCollectionId where
  fromId = NCId
  toId (NCId i) = i
instance IdType DescriptionId where
  fromId = DeId
  toId (DeId i) = i
instance IdType NewDescriptionId where
  fromId = NDId
  toId (NDId i) = i
instance IdType DiscussionId where
  fromId = DiId
  toId (DiId i) = i
instance IdType RelationId where
  fromId = RelId
  toId (RelId i) = i
instance IdType ResultId where
  fromId = ResId
  toId (ResId i) = i
instance IdType UserId where
  fromId = UId
  toId (UId i) = i

instance FromReqURI ArticleId where
  fromReqURI = liftM fromId . fromReqURI
instance FromReqURI CollectionId where
  fromReqURI = liftM fromId . fromReqURI
instance FromReqURI NewCollectionId where
  fromReqURI = liftM fromId . fromReqURI
instance FromReqURI DescriptionId where
  fromReqURI = liftM fromId . fromReqURI
instance FromReqURI NewDescriptionId where
  fromReqURI = liftM fromId . fromReqURI
instance FromReqURI DiscussionId where
  fromReqURI = liftM fromId . fromReqURI
instance FromReqURI RelationId where
  fromReqURI = liftM fromId . fromReqURI
instance FromReqURI ResultId where
  fromReqURI = liftM fromId . fromReqURI
instance FromReqURI UserId where
  fromReqURI = liftM fromId . fromReqURI

instance Show Id where
  show = show . unwrap
instance Show ArticleId where
  show = show . toId
instance Show CollectionId where
  show = show . toId
instance Show NewCollectionId where
  show = show . toId
instance Show DescriptionId where
  show = show . toId
instance Show NewDescriptionId where
  show = show . toId
instance Show DiscussionId where
  show = show . toId
instance Show RelationId where
  show = show . toId
instance Show ResultId where
  show = show . toId
instance Show UserId where
  show = show . toId

instance Read Id where
  readsPrec i = map (first wrap) . readsPrec i
instance Read ArticleId where
  readsPrec i = map (first fromId) . readsPrec i
instance Read CollectionId where
  readsPrec i = map (first fromId) . readsPrec i
instance Read NewCollectionId where
  readsPrec i = map (first fromId) . readsPrec i
instance Read DescriptionId where
  readsPrec i = map (first fromId) . readsPrec i
instance Read NewDescriptionId where
  readsPrec i = map (first fromId) . readsPrec i
instance Read DiscussionId where
  readsPrec i = map (first fromId) . readsPrec i
instance Read RelationId where
  readsPrec i = map (first fromId) . readsPrec i
instance Read ResultId where
  readsPrec i = map (first fromId) . readsPrec i
instance Read UserId where
  readsPrec i = map (first fromId) . readsPrec i
