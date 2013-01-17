module OpenBrain.Data.User where
{-
  This module holds all the data concerning users.
-}
import OpenBrain.Data.Id
import OpenBrain.Data.Hash (Hash)
import OpenBrain.Data.Karma (Karma)
import OpenBrain.Data.TimeFrame

type UserName = String
data UserData = UserData {
    userid       :: UserId
  , username     :: UserName
  , password     :: Hash
  , karma        :: Karma
  , userCreation :: CalendarTime
  , lastLogin    :: CalendarTime
  , isAdmin      :: Bool
  , profile      :: Maybe InformationId
} deriving (Eq, Show)

instance TimeFrame UserData where
  creation = userCreation
  deletion = const Nothing
