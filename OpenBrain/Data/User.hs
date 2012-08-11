module OpenBrain.Data.User where
{-
  This module holds all the data concerning users.
-}
import System.Time (CalendarTime)

import OpenBrain.Data.Id
import OpenBrain.Data.Hash (Hash)
import OpenBrain.Data.Karma (Karma)

type UserName = String
data UserData = UserData {
    userid    :: UserId
  , username  :: UserName
  , password  :: Hash
  , karma     :: Karma
  , creation  :: CalendarTime
  , lastLogin :: CalendarTime
  , isAdmin   :: Bool
} deriving (Eq, Show)

