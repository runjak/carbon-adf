{-# LANGUAGE FlexibleInstances, OverloadedStrings, MultiParamTypeClasses #-}
module Carbon.Data.Result where

import Control.Applicative
import Control.Monad
import Data.Aeson ((.=), ToJSON(..), object, FromJSON(..), Value(..), (.:))
import Data.Function (on)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import qualified Data.List as List

import Carbon.Data.Alias
import Carbon.Data.Common
import Carbon.Data.Id
import Carbon.Data.Logic.Diamond (ResultType(..))

data Result = Result {
  resultId   :: Maybe Id
, resultType :: [ResultType]
, items      :: Set (ResultState, Id)
, votes      :: Votes
} deriving (Show, Read, Eq, Ord)

data ResultState = In | Udec | Out
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

sameResult :: Result -> Result -> Bool
sameResult r1 r2 = let sameTypes = ((==) `on` resultType) r1 r2
                       sameItems = ((==) `on` items) r1 r2
                   in sameTypes && sameItems

-- Instances:
instance ToJSON ResultState where
  toJSON = toJSON . show

instance ToJSON ResultType where
  toJSON = toJSON . show

instance FromJSON Result where
  parseJSON (Object v) = do
    let setI r i = r{resultId = Just i}
        setRT r t = r{resultType = t}
        setIs r i = r{items = i}
        setV r v = r{votes = v}
        parseI r = msum [liftM (setI r) (v .: "id"), return r]
        parseRT r = msum [liftM (setRT r) (v .: "resultType"), return r]
        parseIs r = msum [liftM (setIs r) (v .: "items"), return r]
        parseV r = msum [liftM (setV r) (v .: "votes"), return r]
    r <- parseV =<< parseIs =<< parseRT =<< parseI mempty
    guard $ r /= mempty
    return r
  parseJSON _ = mzero

instance Insertable Result Id where
  r <+ i = r{resultId = Just i}

instance Insertable Result [ResultType] where
  r <+ rs = r{resultType = rs}

instance Insertable Result (Set (ResultState, Id)) where
  r <+ is = r{items = is}

instance Monoid Result where
  mempty = Result {
      resultId   = mempty
    , resultType = mempty
    , items      = mempty
    , votes      = 0
    }
  mappend a b = Result {
      resultId   = (mappend `on` resultId) a b
    , resultType = List.nub $ ((++) `on` resultType) a b
    , items      = items b
    , votes      = votes b
    }

instance FromJSON ResultState where
  parseJSON (String "In")   = return In
  parseJSON (String "Udec") = return Udec
  parseJSON (String "Out")  = return Out
  parseJSON _ = mzero 

instance FromJSON ResultType where
  parseJSON (String "TwoValued")  = return TwoValued
  parseJSON (String "Stable")     = return Stable
  parseJSON (String "Grounded")   = return Grounded
  parseJSON (String "Complete")   = return Complete
  parseJSON (String "Admissible") = return Admissible
  parseJSON (String "Preferred")  = return Preferred
  parseJSON _ = mzero 

instance ToJSON Result where
  toJSON r = object [
      "id"         .= resultId r
    , "resultType" .= resultType r
    , "items"      .= items r
    , "votes"      .= votes r
    ]
