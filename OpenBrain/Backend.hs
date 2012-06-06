module OpenBrain.Backend (
    Backend(..)
  , UserBackend(..)
  , ProfileBackend(..)
  , KarmaBackend(..)
) where
{-
  This module provides the Backend class that will be used to generate the website.
  The Backend will provide things like Userdata :P
-}
import OpenBrain.Config
import OpenBrain.User.Data (UserId, UserData(..), UserName)
import OpenBrain.User.Hash (Hash)
import OpenBrain.User.Karma (Karma)
import OpenBrain.User.Profile (Profile)

{- The highest abstraction of the backend-tree. -}
data Backend = Backend {
    shutdown      :: IO ()
  , userBackend   :: UserBackend
  , karmaBackend  :: KarmaBackend
}

{- Controls for everything userrelated. -}
data UserBackend = UserBackend {
    login           :: UserName -> Hash -> IO (Maybe UserData) -- The Backend will update the lastLogin in UserData.
  , getUser         :: UserId -> IO (Maybe UserData)
  , hasUserWithId   :: UserId -> IO Bool
  , hasUserWithName :: UserName -> IO Bool
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
