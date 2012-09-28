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
import OpenBrain.Data.Relation
import OpenBrain.Data.Salt (Salt)

import qualified OpenBrain.Backend.Types as Types

data Backend = forall b . ( GeneralBackend      b
                          , InformationBackend  b
                          , KarmaBackend        b
                          , RelationBackend     b
                          , SaltShaker          b
                          , SessionManagement   b
                          , UserBackend         b
                          ) => Backend b

{- General Backend control that doesn't belong somewhere else. -}
class GeneralBackend g where
  shutdown :: g -> IO ()

{- Controls for everything userrelated. -}
class UserBackend u where
  login           :: u -> UserName -> Hash -> MaybeT IO UserData -- The Backend will update the lastLogin in UserData.
  getUser         :: u -> UserId -> MaybeT IO UserData
  hasUserWithId   :: u -> UserId -> IO Bool
  hasUserWithName :: u -> UserName -> MaybeT IO UserId
  register        :: u -> UserName -> Hash -> Salt -> MaybeT IO UserData -- The Backend will check for duplicate UserNames.
  delete          :: u -> UserId -> Types.Heir -> IO Bool
  getUserCount    :: u -> IO Types.Count
  getUserList     :: u -> Types.Limit -> Types.Offset -> IO [UserId]
  updateKarma     :: u -> UserId -> (Karma -> Karma) -> IO ()
  updatePasswd    :: u -> UserId -> Hash -> IO ()
  setAdmin        :: u -> UserId -> Bool -> IO ()
  setProfile      :: u -> UserId -> Maybe InformationId -> IO ()

{-
  Controls for everything karma related.
  If Nothing is returned this means that the action is never allowed
  for any amount of karma.
  These are IO so that necessary karma can be computed.
-}
class KarmaBackend k where
  karmaDeleteUser :: k -> IO Karma
  karmaEditUser   :: k -> IO Karma

{-
  The SaltShaker produces Salts
  that can be bound to a UserId
  by using setId.
-}
class SaltShaker s where
  getSalt     :: s -> UserId -> IO Salt

{-
  SessionManagement helps managing logged in clients.
  On Login each client get's an ActionKey.
  The ActionKey shall be stored in a clients cookie along with the clients UserId.
  A client action can than be validated using the clients UserId and ActionKey.
  If the action is valid a new ActionKey will be produced to be stored instead of the old one.
-}

class SessionManagement s where
  startSession  :: s -> UserId -> IO Types.ActionKey
  validate      :: s -> UserId -> Types.ActionKey -> IO Bool
  stopSession   :: s -> UserId -> Types.ActionKey -> IO ()

{-
  Manages all information in the form of OpenBrain.Data.Information
-}
class InformationBackend b where
  -- | 'Creative' Operations:
  addContentMedia   :: b -> Types.CreateInformation -> Types.Content -> IO InformationId
  addToCollection   :: b -> Types.Collection -> [InformationId] -> IO InformationId
  addParticipant    :: b -> InformationId -> UserId -> IO () -- | May only target discussions
  createCollection  :: b -> Types.CreateInformation -> [InformationId] -> IO Types.Collection
  createDiscussion  :: b -> Types.CreateInformation -> [InformationId] -> Types.Deadline -> Types.DiscussionType -> IO InformationId
  -- | 'Querying' Operations:
  getInformationCount         :: b -> IO Types.Count
  getInformation              :: b -> InformationId -> MaybeT IO Information
  getInformations             :: b -> Types.Limit -> Types.Offset -> IO [Information] -- | No parents
  getInformationCountAfter    :: b -> CalendarTime -> IO Types.Count
  getInformationsAfter        :: b -> CalendarTime ->Types.Limit -> Types.Offset -> IO [Information] -- | No parents
  getInformationCountBy       :: b -> UserId -> IO Types.Count
  getInformationBy            :: b -> UserId -> Types.Limit -> Types.Offset -> IO [Information] -- | No parents
  getInformationParentsCount  :: b -> InformationId -> IO Types.Count
  getInformationParents       :: b -> InformationId -> Types.Limit -> Types.Offset -> IO [Information] -- | youngest first
  getProfiledUsers            :: b -> InformationId -> IO [UserData]
  -- | 'Modifying' Operations:
  updateDescription :: b -> InformationId -> Types.Description -> IO InformationId
  updateTitle       :: b -> InformationId -> Types.Title -> IO InformationId
  updateContent     :: b -> InformationId -> Types.Content -> IO InformationId
  vote              :: b -> InformationId -> UserId -> IO () -- | May only target CollectionType Choice - Discussion is found because it's a parent.
  -- | 'Destructive' Operations:
  deleteInformation :: b -> InformationId -> IO () -- | Sets a delete date on an Information
  removeParticipant :: b -> InformationId -> UserId -> IO () -- | May only target discussions, should not be possible when voted.

class RelationBackend b where
  addRelation     :: b -> Types.Source -> Types.Target -> RelationType -> Types.Comment -> IO ()  
  deleteRelation  :: b -> RelationId -> IO ()
  getRelations    :: b -> InformationId -> IO [Relation] -- | youngest first, deleted after non deleted, source = InformationId
  updateComment   :: b -> RelationId -> Types.Comment -> IO ()

