{-# LANGUAGE ExistentialQuantification #-}
module OpenBrain.Backend where
{-
  This module provides the Backend class that will be used to generate the website.
  The Backend will provide things like Userdata :P
-}
import Control.Monad.Trans.Maybe

import OpenBrain.Config
import OpenBrain.Data.User
import OpenBrain.Data.Hash (Hash)
import OpenBrain.Data.Id (Id)
import OpenBrain.Data.Karma (Karma)
import OpenBrain.Data.Profile hiding (emptyProfile)
import OpenBrain.Data.Salt (Salt)

{- The highest abstraction of the backend-tree. -}
class Backend b where
  shutdown          :: b -> IO ()
  userBackend       :: b -> CUserBackend
  karmaBackend      :: b -> CKarmaBackend
  saltShaker        :: b -> CSaltShaker
  sessionManagement :: b -> CSessionManagement
data CBackend = forall b . Backend b => CBackend b
instance Backend CBackend where
  shutdown (CBackend b)           = shutdown b
  userBackend (CBackend b)        = userBackend b
  karmaBackend (CBackend b)       = karmaBackend b
  saltShaker (CBackend b)         = saltShaker b
  sessionManagement (CBackend b)  = sessionManagement b

{- Controls for everything userrelated. -}
type Limit  = Int
type Offset = Int
class UserBackend u where
  login           :: u -> UserName -> Hash -> MaybeT IO UserData -- The Backend will update the lastLogin in UserData.
  getUser         :: u -> UserId -> MaybeT IO UserData
  hasUserWithId   :: u -> UserId -> IO Bool
  hasUserWithName :: u -> UserName -> MaybeT IO UserId
  register        :: u -> UserName -> Hash -> MaybeT IO UserData -- The Backend will check for duplicate UserNames.
  delete          :: u -> UserId -> IO Bool
  profileBackend  :: u -> CProfileBackend
  getUserCount    :: u -> IO Int
  getUserList     :: u -> Limit -> Offset -> IO [UserId]
  updateKarma     :: u -> UserId -> (Karma -> Karma) -> IO ()
  updatePasswd    :: u -> UserId -> Hash -> IO ()
  setAdmin        :: u -> UserId -> Bool -> IO ()
data CUserBackend = forall u . UserBackend u => CUserBackend u
instance UserBackend CUserBackend where
  login (CUserBackend u)            = login u
  getUser (CUserBackend u)          = getUser u
  hasUserWithId (CUserBackend u)    = hasUserWithId u
  hasUserWithName (CUserBackend u)  = hasUserWithName u
  register (CUserBackend u)         = register u
  delete (CUserBackend u)           = delete u
  profileBackend (CUserBackend u)   = profileBackend u
  getUserCount (CUserBackend u)     = getUserCount u
  getUserList (CUserBackend u)      = getUserList u
  updateKarma (CUserBackend u)      = updateKarma u
  updatePasswd (CUserBackend u)     = updatePasswd u
  setAdmin (CUserBackend u)         = setAdmin u

{- Controls for Userprofiles. -}
class ProfileBackend p where
  getProfileId        :: p -> UserId -> IO ProfileId
  getProfile          :: p -> ProfileId -> MaybeT IO Profile
  setAccessRule       :: p -> ProfileId -> AccessRule -> IO ()
  setName             :: p -> ProfileId -> Maybe Name -> IO ()
  setAvatar           :: p -> ProfileId -> Maybe String -> IO ()
  setLocations        :: p -> ProfileId -> [Location] -> IO ()
  setWebsites         :: p -> ProfileId -> [ProfileSnippet] -> IO ()
  setEmails           :: p -> ProfileId -> [ProfileSnippet] -> IO ()
  setInstantMessagers :: p -> ProfileId -> [ProfileSnippet] -> IO ()
data CProfileBackend = forall p . ProfileBackend p => CProfileBackend p
instance ProfileBackend CProfileBackend where
  getProfileId (CProfileBackend p)        = getProfileId p
  getProfile (CProfileBackend p)          = getProfile p
  setAccessRule (CProfileBackend p)       = setAccessRule p
  setName (CProfileBackend p)             = setName p
  setAvatar (CProfileBackend p)           = setAvatar p
  setLocations (CProfileBackend p)        = setLocations p
  setWebsites (CProfileBackend p)         = setWebsites p
  setEmails (CProfileBackend p)           = setEmails p
  setInstantMessagers (CProfileBackend p) = setInstantMessagers p

{-
  Controls for everything karma related.
  If Nothing is returned this means that the action is never allowed
  for any amount of karma.
  These are IO so that necessary karma can be computed.
-}
class KarmaBackend k where
  karmaDeleteUser :: k -> IO Karma
  karmaEditUser   :: k -> IO Karma
data CKarmaBackend = forall k . KarmaBackend k => CKarmaBackend k
instance KarmaBackend CKarmaBackend where
  karmaDeleteUser (CKarmaBackend k) = karmaDeleteUser k
  karmaEditUser (CKarmaBackend k)   = karmaEditUser k

{-
  The SaltShaker produces Salts
  that can be bound to a UserId
  by using setId.
-}
class SaltShaker s where
  setId       :: s -> Salt -> UserId -> IO ()
  getSalt     :: s -> UserId -> IO Salt
  removeSalt  :: s -> UserId -> IO ()
data CSaltShaker = forall s . SaltShaker s => CSaltShaker s
instance SaltShaker CSaltShaker where
  setId (CSaltShaker s)       = setId s
  getSalt (CSaltShaker s)     = getSalt s
  removeSalt (CSaltShaker s)  = removeSalt s

{-
  SessionManagement helps managing logged in clients.
  On Login each client get's an ActionKey.
  The ActionKey shall be stored in a clients cookie along with the clients UserId.
  A client action can than be validated using the clients UserId and ActionKey.
  If the action is valid a new ActionKey will be produced to be stored instead of the old one.
-}
type ActionKey = String
class SessionManagement s where
  startSession  :: s -> UserId -> IO ActionKey
  validate      :: s -> UserId -> ActionKey -> IO Bool
  perform       :: s -> UserId -> ActionKey -> MaybeT IO ActionKey
  stopSession   :: s -> UserId -> ActionKey -> IO ()
data CSessionManagement = forall s . SessionManagement s => CSessionManagement s
instance SessionManagement CSessionManagement where
  startSession (CSessionManagement s) = startSession s
  validate (CSessionManagement s)     = validate s
  perform (CSessionManagement s)      = perform s
  stopSession (CSessionManagement s)  = stopSession s

