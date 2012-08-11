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
import OpenBrain.Data.Id
import OpenBrain.Data.Information
import OpenBrain.Data.Karma (Karma)
import OpenBrain.Data.Profile
import OpenBrain.Data.Relation
import OpenBrain.Data.Salt (Salt)

{- The highest abstraction of the backend-tree. -}
class Backend b where
  informationBackend  :: b -> CInformationBackend
  relationBackend     :: b -> CRelationBackend
  shutdown            :: b -> IO ()
  userBackend         :: b -> CUserBackend
  karmaBackend        :: b -> CKarmaBackend
  saltShaker          :: b -> CSaltShaker
  sessionManagement   :: b -> CSessionManagement
data CBackend = forall b . Backend b => CBackend b
instance Backend CBackend where
  informationBackend  (CBackend b) = informationBackend b
  relationBackend     (CBackend b) = relationBackend b
  shutdown            (CBackend b) = shutdown b
  userBackend         (CBackend b) = userBackend b
  karmaBackend        (CBackend b) = karmaBackend b
  saltShaker          (CBackend b) = saltShaker b
  sessionManagement   (CBackend b) = sessionManagement b

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
  login           (CUserBackend u) = login            u
  getUser         (CUserBackend u) = getUser          u
  hasUserWithId   (CUserBackend u) = hasUserWithId    u
  hasUserWithName (CUserBackend u) = hasUserWithName  u
  register        (CUserBackend u) = register         u
  delete          (CUserBackend u) = delete           u
  profileBackend  (CUserBackend u) = profileBackend   u
  getUserCount    (CUserBackend u) = getUserCount     u
  getUserList     (CUserBackend u) = getUserList      u
  updateKarma     (CUserBackend u) = updateKarma      u
  updatePasswd    (CUserBackend u) = updatePasswd     u
  setAdmin        (CUserBackend u) = setAdmin         u

{- Controls for Userprofiles. -}
class ProfileBackend p where
  getProfile          :: p -> UserId -> IO Profile
  setAccessRule       :: p -> ProfileId -> AccessRule -> IO ()
  setName             :: p -> ProfileId -> Maybe Name -> IO ()
  setAvatar           :: p -> ProfileId -> Maybe String -> IO ()
  setLocations        :: p -> ProfileId -> [Location] -> IO ()
  setWebsites         :: p -> ProfileId -> [ProfileSnippet] -> IO ()
  setEmails           :: p -> ProfileId -> [ProfileSnippet] -> IO ()
  setInstantMessagers :: p -> ProfileId -> [ProfileSnippet] -> IO ()
data CProfileBackend = forall p . ProfileBackend p => CProfileBackend p
instance ProfileBackend CProfileBackend where
  getProfile          (CProfileBackend p) = getProfile          p
  setAccessRule       (CProfileBackend p) = setAccessRule       p
  setName             (CProfileBackend p) = setName             p
  setAvatar           (CProfileBackend p) = setAvatar           p
  setLocations        (CProfileBackend p) = setLocations        p
  setWebsites         (CProfileBackend p) = setWebsites         p
  setEmails           (CProfileBackend p) = setEmails           p
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
  setId       (CSaltShaker s) = setId s
  getSalt     (CSaltShaker s) = getSalt s
  removeSalt  (CSaltShaker s) = removeSalt s

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
  startSession  (CSessionManagement s) = startSession s
  validate      (CSessionManagement s) = validate     s
  perform       (CSessionManagement s) = perform      s
  stopSession   (CSessionManagement s) = stopSession  s

{-
  Manages all information in the form of OpenBrain.Data.Information
-}
type Title = String
type Description = String
type Content = String
class InformationBackend b where
  addContentMedia :: b -> UserId -> Title -> Description -> Content -> IO Information
  
  {- | Below are obsolete, old functions:
  addInformation        :: (UserIdentifier ui) => b -> ui -> Title -> Description -> Media -> IO ()
  deleteInformation     :: (InformationIdentifier i) => b -> i -> IO ()
  getInformationCount   :: b -> IO Int
  getInformation        :: (InformationIdentifier i) => b -> i -> MaybeT IO Information
  getInformations       :: b -> Limit -> Offset -> IO [Information]                 -- | No parents
  getInformationAfter   :: b -> Limit -> CalendarTime -> IO [Information]           -- | No parents
  getInformationBy      :: (UserIdentifier ui) => b -> ui -> IO [Information]       -- | No parents
  getInformationParents :: (InformationIdentifier i) => b -> i -> IO [Information]  -- | youngest first
  updateDescription     :: (InformationIdentifier i, UserIdentifier ui) => b -> ui -> i -> Description -> IO ()
  updateMedia           :: (InformationIdentifier i, UserIdentifier ui) => b -> ui -> i -> Media -> IO ()
  updateTitle           :: (InformationIdentifier i, UserIdentifier ui) => b -> ui -> i -> Title -> IO () -}
data CInformationBackend = forall b . InformationBackend b => CInformationBackend b
instance InformationBackend CInformationBackend where
  addContentMedia = undefined
  -- FIXME implement the rest!

type Source = InformationId
type Target = InformationId
class RelationBackend b where
  addRelation     :: b -> Source -> Target -> RelationType -> String -> IO ()  
  deleteRelation  :: b -> RelationId -> IO ()
  getRelations    :: b -> InformationId -> IO [Relation] -- | youngest first, deleted after non deleted
  updateComment   :: b -> RelationId -> String -> IO ()
data CRelationBackend = forall b . RelationBackend b => CRelationBackend b
instance RelationBackend CRelationBackend where
  addRelation     (CRelationBackend b) = addRelation    b
  deleteRelation  (CRelationBackend b) = deleteRelation b
  getRelations    (CRelationBackend b) = getRelations   b
  updateComment   (CRelationBackend b) = updateComment  b

