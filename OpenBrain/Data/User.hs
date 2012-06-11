{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OpenBrain.Data.User (
    UserData(..)
  , UserName
) where
{-
  This module holds all the data concerning users.
-}
import System.Time (CalendarTime)

import OpenBrain.Data.Id
import OpenBrain.Data.Hash (Hash)
import OpenBrain.Data.Karma (Karma)

type UserName = String
data UserData = UserData {
    userid    :: Id
  , username  :: UserName
  , password  :: Hash
  , karma     :: Karma
  , creation  :: CalendarTime
  , lastLogin :: CalendarTime
  , isAdmin   :: Bool
} deriving (Eq)
