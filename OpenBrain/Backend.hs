module OpenBrain.Backend (
    Backend(..)
  , UserBackend(..)
  , ProfileBackend(..)
  , KarmaBackend(..)
  , SaltShaker(..)
) where
{-
  This module provides the Backend class that will be used to generate the website.
  The Backend will provide things like Userdata :P
-}
import OpenBrain.Config
import OpenBrain.Data.User (UserData(..), UserId, UserName)
import OpenBrain.Data.Hash (Hash)
import OpenBrain.Data.Id (Id)
import OpenBrain.Data.Karma (Karma)
import OpenBrain.Data.Profile (Profile)
import OpenBrain.Data.Salt (Salt)

{- The highest abstraction of the backend-tree. -}
data Backend = Backend {
    shutdown      :: IO ()
  , userBackend   :: UserBackend
  , karmaBackend  :: KarmaBackend
  , saltShaker    :: SaltShaker
}

{- Controls for everything userrelated. -}
data UserBackend = UserBackend {
    login           :: UserName -> Hash -> IO (Maybe UserData) -- The Backend will update the lastLogin in UserData.
  , getUser         :: UserId -> IO (Maybe UserData)
  , hasUserWithId   :: UserId -> IO Bool
  , hasUserWithName :: UserName -> IO (Maybe UserId)
  , register        :: UserName -> Hash -> IO (Maybe UserData) -- The Backend will check for duplicate UserNames.
  , delete          :: UserId -> IO Bool
  , profileBackend  :: ProfileBackend
}

{- Controls for Userprofiles. -}
data ProfileBackend = ProfileBackend {
    getProfile :: UserId -> IO (Maybe Profile)
  , setProfile :: UserId -> Profile -> IO Bool
  -- FIXME Update Name, etc…
}

{-
  Controls for everything karma related.
  If Nothing is returned this means that the action is never allowed
  for any amount of karma.
  These are IO so that necessary karma can be computed.
-}
data KarmaBackend = KarmaBackend {
    karmaDeleteUser :: IO Karma
  , karmaEditUser   :: IO Karma
}

{-
  The SaltShaker produces Salts
  that can be bound to a UserId
  by using setId.
-}
data SaltShaker = SaltShaker {
    setId :: Salt -> UserId -> IO () -- Stores a tuple of Salt and Id in the Backend.
  , getSalt :: UserId -> IO Salt -- May use shake and setId of no Salt exists.
  , removeSalt :: UserId -> IO () -- Deleting the Salt when it's not necessary anymore.
}
