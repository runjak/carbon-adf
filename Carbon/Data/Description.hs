{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Carbon.Data.Description where

import Control.Applicative
import Control.Monad
import Data.Aeson ((.=), ToJSON(..), object, FromJSON(..), Value(..), (.:))
import Data.Function (on)
import Data.Monoid (Monoid(..))

import Carbon.Data.Common
import Carbon.Data.Alias
import Carbon.Data.Id

data Description = Description {
  descriptionId :: Maybe Id
, headline      :: Headline
, summary       :: Summary
} deriving (Show, Read, Eq, Ord)

-- Instances:
instance FromJSON Description where
  parseJSON (Object v) = do
    let setI d i = d{descriptionId=i}
        setH d h = d{headline=h}
        setS d s = d{summary=s}
        parseI d = msum [liftM (setI d) (v .: "id"), return d]
        parseH d = msum [liftM (setH d) (v .: "headline"), return d]
        parseS d = msum [liftM (setS d) (v .: "summary"), return d]
    d <- parseS =<< parseH =<< parseI mempty
    guard $ d /= mempty
    return d
  parseJSON _ = mzero

instance Insertable Description Id where
  d <+ i = d{descriptionId = Just i}

instance Monoid Description where
  mempty = Description {
      descriptionId = mempty
    , headline      = ""
    , summary       = ""
    }
  mappend a b = Description {
      descriptionId = (mappend `on` descriptionId) a b
    , headline = (mergeStr `on` headline) a b
    , summary  = (mergeStr `on` summary) a b
    }

instance ToJSON Description where
  toJSON d = object [
      "id"       .= descriptionId d
    , "headline" .= headline d
    , "summary"  .= summary d
    ]
