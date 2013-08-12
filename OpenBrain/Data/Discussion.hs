{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module OpenBrain.Data.Discussion where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Function (on)

import OpenBrain.Data.Alias
import OpenBrain.Data.Collection
import OpenBrain.Data.Id
import OpenBrain.Data.Json
import OpenBrain.Data.Relation
import OpenBrain.Data.Result

data Discussion expType = Discussion {
  discussionId :: DiscussionId
, participants :: [(UserId, Voted)]
, deadline     :: Maybe Timestamp
, relations    :: [Relation]
, results      :: [Result]
, dCollection  :: Collection expType
}

instance Eq (Discussion e) where
  (==) = (==) `on` discussionId
instance Functor Discussion where
  fmap f d = d{dCollection = fmap f $ dCollection d}
instance Ord (Discussion e) where
  compare = compare `on` discussionId
instance Show (Discussion Headline) where
  show d =
    let x1 = "discussionId=" ++ show (discussionId d)
        x2 = ",participants=" ++ show (participants d)
        x3 = ",deadline=" ++ show (deadline d)
        x4 = ",realtions=" ++ show (relations d)
        x5 = ",results=" ++ show (results d)
        x6 = ",dCollection=" ++ show (dCollection d)
    in "Discussion {"++x1++x2++x3++x4++x5++x6++"}"
instance ToJSON (Discussion Headline) where
  toJSON d = merge (toJSON $ dCollection d) o
    where
      o = object [
          "id"           .= discussionId d
        , "participants" .= participants d
        , "deadline"     .= deadline     d
        , "relations"    .= relations    d
        , "results"      .= results      d
        ]
