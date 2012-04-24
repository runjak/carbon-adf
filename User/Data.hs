{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module User.Data (
    UserId
  , toUserId
  , fromUserId
  , Userdata(..)
) where
{-
  This module holds all the data concerning users.
-}
import User.Hash (Hash)
import User.Karma (Karma)

newtype UserId = UserId Integer deriving (Eq, Ord, Enum, Read, Show)

toUserId :: Integer -> UserId
toUserId = UserId

fromUserId :: UserId -> Integer
fromUserId (UserId i) = i

data Userdata = Userdata {
    userid    :: UserId
  , username  :: String
  , password  :: Hash
  , karma     :: Karma
} deriving (Eq)
