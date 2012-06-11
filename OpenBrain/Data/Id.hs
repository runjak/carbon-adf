{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OpenBrain.Data.Id (
  Id, toId, fromId
) where

import Data.Aeson (ToJSON, FromJSON)

newtype Id = Id Integer
  deriving (Eq, Ord, Enum, Read, Show, ToJSON, FromJSON)

toId :: Integer -> Id
toId = Id

fromId :: Id -> Integer
fromId (Id i) = i
