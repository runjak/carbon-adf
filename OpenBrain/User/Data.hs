{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OpenBrain.User.Data (
    UserId
  , toUserId
  , fromUserId
  , UserData(..)
  , UserName
) where
{-
  This module holds all the data concerning users.
-}
import OpenBrain.User.Hash (Hash)
import OpenBrain.User.Karma (Karma)

newtype UserId = UserId Integer deriving (Eq, Ord, Enum, Read, Show)

toUserId :: Integer -> UserId
toUserId = UserId

fromUserId :: UserId -> Integer
fromUserId (UserId i) = i

type UserName = String
data UserData = Userdata {
    userid    :: UserId
  , username  :: UserName
  , password  :: Hash
  , karma     :: Karma
} deriving (Eq)
