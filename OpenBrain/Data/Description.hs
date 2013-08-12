{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Data.Description where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Function (on)

import OpenBrain.Data.Alias
import OpenBrain.Data.Id

data Description = Description {
  descriptionId :: DescriptionId
, author        :: Author
, headline      :: Headline
, description   :: String
, creationTime  :: Timestamp
, deletionTime  :: Maybe Timestamp
} deriving (Show)

instance Eq Description where
  (==) = (==) `on` descriptionId
instance Ord Description where
  compare = compare `on` descriptionId
instance ToJSON Description where
  toJSON d = object [
      "descriptionId" .= descriptionId d
    , "author"        .= author        d
    , "headline"      .= headline      d
    , "description"   .= description   d
    , "creationTime"  .= creationTime  d
    , "deletionTime"  .= deletionTime  d
    ]
