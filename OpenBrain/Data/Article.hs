{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Data.Article where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Function (on)

import OpenBrain.Data.Description
import OpenBrain.Data.Id
import OpenBrain.Data.Json

data Article = Article {
  articleId    :: ArticleId
, content      :: String
, children     :: [ArticleId]
, parents      :: [ArticleId]
, aDescription :: Description
} deriving (Show)

isDummy :: Article -> Bool
isDummy a = all null [content a, description $ aDescription a]

instance Eq Article where
  (==) = (==) `on` articleId
instance Ord Article where
  compare = compare `on` articleId
instance ToJSON Article where
  toJSON a = merge (toJSON $ aDescription a) o
    where
      o = object [
          "id"       .= articleId a
        , "content"  .= content   a
        , "children" .= children  a
        , "parents"  .= parents   a
        ]
