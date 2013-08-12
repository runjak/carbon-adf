{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Data.Result where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Function (on)

import OpenBrain.Data.Alias
import OpenBrain.Data.Id
import OpenBrain.Data.Logic

data ResultState = In | Udec | Out
  deriving (Bounded, Eq, Enum, Ord, Show)

data Result = Result {
  resultId   :: ResultId
, resultType :: ResultType
, rArticles  :: [(ResultState, ArticleId)]
, votes      :: Votes
} deriving (Show)

instance Eq Result where
  (==) = (==) `on` resultId
instance Ord Result where
  compare = compare `on` resultId
instance ToJSON ResultState where
  toJSON = toJSON . show
instance ToJSON ResultType where
  toJSON = toJSON . show
instance ToJSON Result where
  toJSON r = object [
      "id"       .= resultId   r
    , "type"     .= resultType r
    , "articles" .= rArticles  r
    , "votes"    .= votes      r
    ]
