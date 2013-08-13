{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module OpenBrain.Data.CollectionArticle where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Function (on)

import OpenBrain.Data.Alias
import OpenBrain.Data.Article
import OpenBrain.Data.Logic
import OpenBrain.Data.Json

data CollectionArticle expType = CollectionArticle {
  cArticle        :: Article
, posX            :: Int
, posY            :: Int
, accepted        :: Maybe Bool
, customcondition :: Bool
, condition       :: Exp expType
}

instance Eq (CollectionArticle e) where
  (==) = (==) `on` cArticle

instance Functor CollectionArticle where
  fmap f ca = ca{condition = fmap f $ condition ca}

instance Ord (CollectionArticle e) where
  compare = compare `on` cArticle

instance Show (CollectionArticle Headline) where
  show ca  = 
    let x1 = "cArticle="         ++ show (cArticle ca)
        x2 = ",posX="            ++ show (posX ca)
        x3 = ",posY="            ++ show (posY ca)
        x4 = ",accepted="        ++ show (accepted ca)
        x5 = ",customcondition=" ++ show (customcondition ca)
        x6 = ",condition="       ++ show (condition ca)
    in "CollectionArticle {"++x1++x2++x3++x4++x5++x6++"}"

instance ToJSON (CollectionArticle Headline) where
  toJSON c = merge (toJSON $ cArticle c) o
    where 
      o = object [
          "posX"            .= posX            c
        , "posY"            .= posY            c
        , "accepted"        .= accepted        c
        , "customcondition" .= customcondition c
        , "condition"       .= show (condition c)
        ]

instance VarContainer CollectionArticle where
  vars = vars . condition
