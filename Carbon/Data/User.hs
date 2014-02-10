{-# LANGUAGE OverloadedStrings #-}
module Carbon.Data.User where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Function (on)

import Carbon.Data.Alias
import Carbon.Data.Id
import Carbon.Data.Hash
import Carbon.Data.Salt

data User = User {
  userId       :: Id
, username     :: String
, userhash     :: Hash
, usersalt     :: Salt
, userCreation :: Timestamp
, lastLogin    :: Timestamp
, isAdmin      :: Bool
, profile      :: Maybe Id
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
