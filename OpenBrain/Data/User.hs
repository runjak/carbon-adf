{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Data.User where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Function (on)

import OpenBrain.Data.Alias
import OpenBrain.Data.Id
import OpenBrain.Data.Hash
import OpenBrain.Data.Salt

data User = User {
  userId       :: UserId
, username     :: String
, userhash     :: Hash
, usersalt     :: Salt
, userCreation :: Timestamp
, lastLogin    :: Timestamp
, isAdmin      :: Bool
, profile      :: Maybe ArticleId
, session      :: Maybe SessionKey
} deriving (Show)

instance Eq User where
  (==) = (==) `on` userId
instance Ord User where
  compare = compare `on` userId
instance ToJSON User where
  toJSON u = object [
      "id"           .= userId       u
    , "username"     .= username     u
    , "userCreation" .= userCreation u
    , "lastLogin"    .= lastLogin    u
    , "isAdmin"      .= isAdmin      u
    , "profile"      .= profile      u
    ]
