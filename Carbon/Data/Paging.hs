{-# LANGUAGE OverloadedStrings #-}
module Carbon.Data.Paging where

import Control.Applicative
import Control.Monad
import Data.Aeson ((.=), ToJSON(..), object, FromJSON(..), Value(..), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe

import Carbon.Common
import Carbon.Data.Alias
import Carbon.Data.Json (merge)

data Paging = Paging {
  isArticle    :: Maybe Bool
, isDeleted    :: Maybe Bool
, isDiscussion :: Maybe Bool
, isRelation   :: Maybe Bool
, isResult     :: Maybe Bool
, limit        :: Count
, offset       :: Count
} deriving (Show, Read, Eq, Ord)

defaultPaging = Paging {
  isArticle    = Just False
, isDeleted    = Just False
, isDiscussion = Just False
, isRelation   = Just False
, isResult     = Just False
, limit        = 30
, offset       = 0
}

instance ToJSON Paging where
  toJSON p = let mkPair k v = Maybe.isJust v ? ([k .= Maybe.fromJust v],[])
                 article    = mkPair "isArticle"    $ isArticle    p
                 deleted    = mkPair "isDeleted"    $ isDeleted    p
                 discussion = mkPair "isDiscussion" $ isDiscussion p
                 relation   = mkPair "isRelation"   $ isRelation   p
                 result     = mkPair "isResult"     $ isResult     p
             in object $ [
                  "limit"  .= limit  p
                , "offset" .= offset p
                ] ++ concat [article, deleted, discussion, relation, result]

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
