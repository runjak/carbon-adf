{-# LANGUAGE OverloadedStrings #-}
module Carbon.Data.Paging where

import Control.Applicative
import Control.Monad
import Data.Aeson ((.=), ToJSON(..), object, FromJSON(..), Value(..), (.:))
import qualified Data.Aeson as Aeson

import Carbon.Data.Alias
import Carbon.Data.Json (merge)

data Paging = Paging {
  isArticle    :: Bool
, isDeleted    :: Bool
, isDiscussion :: Bool
, isRelation   :: Bool
, isResult     :: Bool
, limit        :: Count
, offset       :: Count
} deriving (Show, Read, Eq, Ord)

defaultPaging = Paging {
  isArticle    = False
, isDeleted    = False
, isDiscussion = False
, isRelation   = False
, isResult     = False
, limit        = 30
, offset       = 0
}

instance ToJSON Paging where
  toJSON p = object [
      "isArticle"    .= isArticle p
    , "isDeleted"    .= isDeleted p
    , "isDiscussion" .= isDiscussion p
    , "isRelation"   .= isRelation p
    , "isResult"     .= isResult p
    , "limit"        .= limit p
    , "offset"       .= offset p
    ]

instance FromJSON Paging where
  parseJSON o'@(Object _) = let (Object o) = merge o' $ toJSON defaultPaging
                            in Paging
                              <$> o .: "isArticle"
                              <*> o .: "isDeleted"
                              <*> o .: "isDiscussion"
                              <*> o .: "isRelation"
                              <*> o .: "isResult"
                              <*> o .: "limit"
                              <*> o .: "offset"
  parseJSON _ = mzero
