{-# LANGUAGE ExistentialQuantification #-}
module OpenBrain.Backend where
{-
  This module provides the Backend class that will be used to generate the website.
  The Backend will provide things like Userdata :P
-}
import Control.Monad.Trans.Maybe
import System.Time (CalendarTime)

import OpenBrain.Config
import OpenBrain.Data.User
import OpenBrain.Data.Hash (Hash)
import OpenBrain.Data.Id (Id)
import OpenBrain.Data.Information
import OpenBrain.Data.Karma (Karma)
import OpenBrain.Data.Profile
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
  getUser         :: (UserIdentifier ui) => u -> ui -> MaybeT IO UserData
  hasUserWithId   :: (UserIdentifier ui) => u -> ui -> IO Bool
  hasUserWithName :: u -> UserName -> MaybeT IO UserId
  register        :: u -> UserName -> Hash -> MaybeT IO UserData -- The Backend will check for duplicate UserNames.
  delete          :: (UserIdentifier ui) => u -> ui -> IO Bool
  profileBackend  :: u -> CProfileBackend
  getUserCount    :: u -> IO Int
  getUserList     :: u -> Limit -> Offset -> IO [UserId]
  updateKarma     :: (UserIdentifier ui) => u -> ui -> (Karma -> Karma) -> IO ()
  updatePasswd    :: (UserIdentifier ui) => u -> ui -> Hash -> IO ()
  setAdmin        :: (UserIdentifier ui) => u -> ui -> Bool -> IO ()
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
  getProfile          :: (UserIdentifier ui) => p -> ui -> IO Profile
  setAccessRule       :: (ProfileIdentifier pi) => p -> pi -> AccessRule -> IO ()
  setName             :: (ProfileIdentifier pi) => p -> pi -> Maybe Name -> IO ()
  setAvatar           :: (ProfileIdentifier pi) => p -> pi -> Maybe String -> IO ()
  setLocations        :: (ProfileIdentifier pi) => p -> pi -> [Location] -> IO ()
  setWebsites         :: (ProfileIdentifier pi) => p -> pi -> [ProfileSnippet] -> IO ()
  setEmails           :: (ProfileIdentifier pi) => p -> pi -> [ProfileSnippet] -> IO ()
  setInstantMessagers :: (ProfileIdentifier pi) => p -> pi -> [ProfileSnippet] -> IO ()
data CProfileBackend = forall p . ProfileBackend p => CProfileBackend p
instance ProfileBackend CProfileBackend where
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
  setId       :: (UserIdentifier ui) => s -> Salt -> ui -> IO ()
  getSalt     :: (UserIdentifier ui) => s -> ui -> IO Salt
  removeSalt  :: (UserIdentifier ui) => s -> ui -> IO ()
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
  startSession  :: (UserIdentifier ui) => s -> ui -> IO ActionKey
  validate      :: (UserIdentifier ui) => s -> ui -> ActionKey -> IO Bool
  perform       :: (UserIdentifier ui) => s -> ui -> ActionKey -> MaybeT IO ActionKey
  stopSession   :: (UserIdentifier ui) => s -> ui -> ActionKey -> IO ()
data CSessionManagement = forall s . SessionManagement s => CSessionManagement s
instance SessionManagement CSessionManagement where
  startSession (CSessionManagement s) = startSession s
  validate (CSessionManagement s)     = validate s
  perform (CSessionManagement s)      = perform s
  stopSession (CSessionManagement s)  = stopSession s

{-
  Manages all information in the form of OpenBrain.Data.Information
-}
class InformationBackend b where
  getInformationCount :: b -> IO Int
  getInformation :: b -> Limit -> Offset -> IO [Information]
  getInformationBy :: (UserIdentifier ui) => b -> ui -> IO [Information]
  getInformationAfter :: b -> Limit -> CalendarTime -> IO [Information]
  -- FIXME add to declaration

