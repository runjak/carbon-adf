{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Carbon.Data.Article where

import Control.Applicative
import Control.Monad
import Data.Aeson ((.=), ToJSON(..), object, FromJSON(..), Value(..), (.:))
import Data.Monoid (Monoid(..))
import Data.Function (on)

import Carbon.Data.Common
import Carbon.Data.Alias
import Carbon.Data.Id

data Article = Article {
  articleId :: Maybe Id
, content   :: Content
} deriving (Show, Read, Eq, Ord)

-- Instances:
instance FromJSON Article where
  parseJSON (Object v) = do
    let setI a i = a{articleId = Just i}
        setC a c = a{content = c}
        parseI a = msum [liftM (setI a) (v .: "id"), return a]
        parseC a = msum [liftM (setC a) (v .: "content"), return a]
    i <- parseC =<< parseI mempty
    guard $ i /= mempty
    return i
  parseJSON _ = mzero

instance Insertable Article Id where
  a <+ i = a{articleId = Just i}

instance Monoid Article where
  mempty = Article {
      articleId = mempty
    , content   = ""
    }
  mappend a b = Article {
      articleId = (mappend `on` articleId) a b
    , content   = (mergeStr `on` content) a b
    }

instance ToJSON Article where
  toJSON a = object [
      "id"      .= articleId a
    , "content" .= content a
    ]
