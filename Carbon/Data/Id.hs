{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Carbon.Data.Id where

import Control.Arrow (first)
import Data.Aeson (ToJSON, FromJSON)
import Data.Function(on)
import Data.Monoid (Monoid(..))
import Happstack.Server as S

-- The Id type used everywhere in the system
newtype Id = Id Integer
  deriving (Eq, Ord, Enum, ToJSON, FromJSON)

wrap :: Integer -> Id
wrap = Id

unwrap :: Id -> Integer
unwrap (Id i) = i

noId :: Maybe Id -> Bool
noId Nothing = True
noId (Just i) = i == mempty

-- Means to compare and order things that have an Id
class HasId i where
  getId  :: i -> Id

sameId :: HasId i => i -> i -> Bool
sameId = (==) `on` getId

compareId :: HasId i => i -> i -> Ordering
compareId = compare `on` getId

-- Instances:
instance Monoid Id where
  mempty = wrap $ -1
  mappend a b
    | b == mempty = a
    | otherwise   = b

instance FromReqURI Id where
  fromReqURI = return . wrap . read

instance Read Id where
  readsPrec = go . readsPrec
    where
      go :: ReadS Integer -> ReadS Id
      go = (.) (map $ first Id)

instance Show Id where
  show (Id i) = show i

instance ToMessage Id where
  toContentType = const "application/json"
  toMessage     = toMessage . show . unwrap
  toResponse    = toResponse . show . unwrap
