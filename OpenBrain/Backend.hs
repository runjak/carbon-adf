module OpenBrain.Backend (
    Backend(..)
  , UserBackend(..)
) where
{-
  This module provides the Backend class that will be used to generate the website.
  The Backend will provide things like Userdata :P
-}
import OpenBrain.Config
import OpenBrain.User.Hash (Hash)
import OpenBrain.User.Data (UserId, UserData(..), UserName)

data Backend = Backend {
  userBackend :: UserBackend
}

data UserBackend = UserBackend {
    login           :: UserName -> Hash -> IO (Maybe UserData)
  , getUser         :: UserId -> IO (Maybe UserData)
  , hasUserWithId   :: UserId -> IO Bool
  , hasUserWithName :: UserName -> IO Bool
  , register        :: UserName -> Hash -> IO (Maybe UserData)
  , delete          :: UserId -> IO Bool
}
