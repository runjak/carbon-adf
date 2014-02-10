{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Carbon.Data.Relation where

import Control.Applicative
import Control.Monad
import Data.Aeson ((.=), ToJSON(..), object, FromJSON(..), Value(..), (.:))
import Data.Function (on)
import Data.Monoid (Monoid(..))

import Carbon.Data.Alias
import Carbon.Data.Common
import Carbon.Data.Id

data Relation = Relation {
  relationId   :: Maybe Id
, source       :: ItemId
, target       :: ItemId
, relationType :: RelationType
} deriving (Show, Read, Eq, Ord)

data RelationType = RelationAttack
                  | RelationSupport
                  | RelationCustom
                  deriving (Show, Read, Ord, Eq, Enum)

-- Instances:
instance FromJSON RelationType where
  parseJSON (String "RelationAttack") = return RelationAttack
  parseJSON (String "RelationSupport") = return RelationSupport
  parseJSON (String "RelationCustom") = return RelationCustom
  parseJSON _ = mzero

instance FromJSON Relation where
  parseJSON (Object v) = do
    let setI r i = r{relationId = Just i}
        setS r s = r{source = s}
        setT r t = r{target = t}
        setR r t = r{relationType = t}
        parseI r = msum [liftM (setI r) (v .: "id"), return r]
        parseS r = msum [liftM (setS r) (v .: "source"), return r]
        parseT r = msum [liftM (setT r) (v .: "target"), return r]
        parseR r = msum [liftM (setR r) (v .: "relationType"), return r]
    r <- parseR =<< parseT =<< parseS =<< parseI mempty
    guard $ r /= mempty
    return r
  parseJSON _ = mzero

instance Insertable Relation Id where
  r <+ i = r{relationId = Just i}

instance Insertable Relation RelationType where
  r <+ rt = r{relationType = rt}

instance Monoid Relation where
  mempty = Relation {
      relationId   = mempty
    , source       = mempty
    , target       = mempty
    , relationType = RelationAttack
    }
  mappend a b = Relation {
      relationId   = (mappend `on` relationId) a b
    , source       = (mappend `on` source) a b
    , target       = (mappend `on` target) a b
    , relationType = relationType b
    }

instance ToJSON Relation where
  toJSON r = object [
      "id"           .= relationId r
    , "source"       .= source r
    , "target"       .= target r
    , "relationType" .= relationType r
    ]

instance ToJSON RelationType where
  toJSON = toJSON . show
