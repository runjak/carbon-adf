{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OpenBrain.Data.Id (
    Id, wrap, unwrap
  , IdType(..)
  , InformationId, RelationId, UserId
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

newtype InformationId = IId Id deriving (Eq, Ord, Enum, Read, Show, ToJSON, FromJSON)
newtype RelationId    = RId Id deriving (Eq, Ord, Enum, Read, Show, ToJSON, FromJSON)
newtype UserId        = UId Id deriving (Eq, Ord, Enum, Read, Show, ToJSON, FromJSON)

instance IdType InformationId where
  fromId        = IId
  toId (IId i)  = i

instance IdType RelationId where
  fromId        = RId
  toId (RId i)  = i

instance IdType UserId where
  fromId        = UId
  toId (UId i)  = i

-- | To make generic handling of IdType easier
instance IdType Id where
  fromId  = id
  toId    = id

-- | Instances for external classes:
instance FromReqURI Id where
  fromReqURI = return . wrap . read
