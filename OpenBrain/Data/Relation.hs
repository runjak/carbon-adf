{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Data.Relation where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Function (on)

import OpenBrain.Data.Description
import OpenBrain.Data.Id
import OpenBrain.Data.Json

data Relation = Relation {
  relationId   :: RelationId
, source       :: ArticleId
, target       :: ArticleId
, rDescription :: Description
} deriving (Show)

instance Eq Relation where
  (==) = (==) `on` relationId
instance Ord Relation where
  compare = compare `on` relationId
instance ToJSON Relation where
  toJSON r = merge (toJSON $ rDescription r) o
    where
      o = object [
          "id"     .= relationId r
        , "source" .= source     r
        , "target" .= target     r
        ]
