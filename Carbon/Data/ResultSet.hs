{-# LANGUAGE FlexibleInstances, OverloadedStrings, MultiParamTypeClasses, TypeSynonymInstances #-}
module Carbon.Data.ResultSet where

import Control.Applicative
import Control.Monad
import Data.Aeson ((.=), ToJSON(..), object, FromJSON(..), Value(..), (.:))
import Data.Function (on)
import Data.Monoid (Monoid(..))
import qualified Data.Set as Set

import Carbon.Data.Alias
import Carbon.Data.Common
import Carbon.Data.Id
import Carbon.Data.Logic.Diamond
import Carbon.Data.Result

data ResultSet = ResultSet {
  resultSetId :: Maybe ResultSetId
, setCreation :: Timestamp
, results     :: [Result]
, voters      :: [(UserId, Voted)]
} deriving (Show, Read, Eq, Ord)

fromResults :: Results ItemId -> ResultSet
fromResults (Results rs) = mempty <+ concatMap go rs
  where
    go :: (ResultType, [DiamondResult ItemId]) -> [Result]
    go (rType, dRes) = map (go2 $ implications rType) dRes

    go2 :: [ResultType] -> DiamondResult ItemId -> Result
    go2 rTypes dRes =
      let ins   = zip (repeat   In) $ inSet   dRes
          udecs = zip (repeat Udec) $ udecSet dRes
          outs  = zip (repeat  Out) $ outSet  dRes
          iSet  = Set.fromList $ ins ++ udecs ++ outs
      in (mempty <+ rTypes) <+ iSet

-- Instances:
instance FromJSON ResultSet where
  parseJSON (Object v) = do
    let setI r i = r{resultSetId = Just i}
        setS r s = r{setCreation = s}
        setR r s = r{results = s}
        setV r v = r{voters = v}
        parseI r = msum [liftM (setI r) (v .: "id"), return r]
        parseS r = msum [liftM (setS r) (v .: "setCreation"), return r]
        parseR r = msum [liftM (setR r) (v .: "results"), return r]
        parseV r = msum [liftM (setV r) (v .: "voters"), return r]
    r <- parseV =<< parseR =<< parseS =<< parseI mempty
    guard $ r /= mempty
    return r
  parseJSON _ = mzero

instance Insertable ResultSet ResultSetId where
  r <+ i = r{resultSetId = Just i}

instance Insertable ResultSet [Result] where
  r <+ rs = r{results = rs}

instance Monoid ResultSet where
  mempty = ResultSet {
      resultSetId = mempty
    , setCreation = mempty
    , results     = mempty
    , voters      = mempty
    }
  mappend a b = ResultSet {
      resultSetId = (mappend `on` resultSetId) a b
    , setCreation = (mergeStr `on` setCreation) a b
    , results     = results b
    , voters      = voters b
    }

instance ToJSON ResultSet where
  toJSON r = object [
      "id"          .= resultSetId r
    , "setCreation" .= setCreation r
    , "results"     .= results r
    , "voters"      .= voters r
    ]
