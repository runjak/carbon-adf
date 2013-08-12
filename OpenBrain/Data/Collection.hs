{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module OpenBrain.Data.Collection where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Function (on)

import OpenBrain.Data.Alias
import OpenBrain.Data.CollectionArticle
import OpenBrain.Data.Description
import OpenBrain.Data.Id
import OpenBrain.Data.Json

data Collection expType = Collection {
  collectionId :: CollectionId
, articles     :: [CollectionArticle expType]
, cDescription :: Description
}

instance Eq (Collection e) where
  (==) = (==) `on` collectionId
instance Functor Collection where
  fmap f c = c{articles = map (fmap f) $ articles c}
instance Ord (Collection e) where
  compare = compare `on` collectionId
instance Show (Collection Headline) where
  show c =
    let x1 = "collectionId=" ++ show (collectionId c)
        x2 = ",articles="    ++ show (articles c)
        x3 = ",cDescription" ++ show (cDescription c)
    in "Collection {"++x1++x2++x3++"}"
instance ToJSON (Collection Headline) where
  toJSON c = merge (toJSON $ cDescription c) o
    where
      o = object ["collectionId" .= collectionId c, "articles" .= articles c]
