module OpenBrain.Backend (
    Backend(..)
  , UserBackend(..)
  , ProfileBackend(..)
  , KarmaBackend(..)
  , SaltShaker(..)
  , SessionManagement(..), ActionKey
) where
{-
  This module provides the Backend class that will be used to generate the website.
  The Backend will provide things like Userdata :P
-}
import OpenBrain.Config
import OpenBrain.Data.User
import OpenBrain.Data.Hash (Hash)
import OpenBrain.Data.Id (Id)
import OpenBrain.Data.Karma (Karma)
import OpenBrain.Data.Profile hiding (emptyProfile)
import OpenBrain.Data.Salt (Salt)

{- The highest abstraction of the backend-tree. -}
data Backend = Backend {
    shutdown          :: IO ()
  , userBackend       :: UserBackend
  , karmaBackend      :: KarmaBackend
  , saltShaker        :: SaltShaker
  , sessionManagement :: SessionManagement
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
  , getUserList     :: IO [UserId]
  , updateKarma     :: UserId -> (Karma -> Karma) -> IO ()
  , updatePasswd    :: UserId -> Hash -> IO ()
  , setAdmin        :: UserId -> Bool -> IO ()
}

{- Controls for Userprofiles. -}
data ProfileBackend = ProfileBackend {
    getProfileId        :: UserId -> IO ProfileId
  , getProfile          :: ProfileId -> IO (Maybe Profile)
  , setAccessRule       :: ProfileId -> AccessRule -> IO ()
  , setName             :: ProfileId -> Maybe Name -> IO ()
  , setAvatar           :: ProfileId -> Maybe String -> IO ()
  , setLocations        :: ProfileId -> [Location] -> IO ()
  , setWebsites         :: ProfileId -> [ProfileSnippet] -> IO ()
  , setEmails           :: ProfileId -> [ProfileSnippet] -> IO ()
  , setInstantMessagers :: ProfileId -> [ProfileSnippet] -> IO ()
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

{-
  SessionManagement helps managing logged in clients.
  On Login each client get's an ActionKey.
  The ActionKey shall be stored in a clients cookie along with the clients UserId.
  A client action can than be validated using the clients UserId and ActionKey.
  If the action is valid a new ActionKey will be produced to be stored instead of the old one.
-}
type ActionKey = String
data SessionManagement = SessionManagement {
    startSession  :: UserId -> IO ActionKey
  , validate      :: UserId -> ActionKey -> IO Bool
  , perform       :: UserId -> ActionKey -> IO (Maybe ActionKey)
  , stopSession   :: UserId -> ActionKey -> IO ()
}
