{-# LANGUAGE ExistentialQuantification #-}
module OpenBrain.Backend where
{-
  This module provides the Backend class that will be used to generate the website.
  The Backend will provide things like Userdata :P
-}
import Control.Monad.Trans.Maybe

import OpenBrain.Config
import OpenBrain.Data

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
  login           :: UserName -> Hash -> u -> IO (Maybe UserData) -- The Backend will update the lastLogin in UserData.
  getUser         :: UserId -> u -> IO (Maybe UserData)
  getNobody       :: u -> IO UserId
  hasUserWithId   :: UserId -> u -> IO Bool
  hasUserWithName :: UserName -> u -> IO (Maybe UserId)
  register        :: UserName -> Hash -> Salt -> u -> IO (Maybe UserData) -- The Backend will check for duplicate UserNames.
  delete          :: UserId -> Heir -> u -> IO Bool
  getUserCount    :: u -> IO Count
  getUserList     :: Limit -> Offset -> u -> IO [UserId]
  updateKarma     :: UserId -> (Karma -> Karma) -> u -> IO ()
  updatePasswd    :: UserId -> Hash -> u -> IO ()
  setAdmin        :: UserId -> Bool -> u -> IO ()
  setProfile      :: UserId -> Maybe InformationId -> u -> IO ()

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
  getSalt     :: UserId -> s -> IO Salt

{-
  SessionManagement helps managing logged in clients.
  On Login each client get's an ActionKey.
  The ActionKey shall be stored in a clients cookie along with the clients UserId.
  A client action can than be validated using the clients UserId and ActionKey.
  If the action is valid a new ActionKey will be produced to be stored instead of the old one.
-}

class SessionManagement s where
  startSession  :: UserId -> s -> IO ActionKey
  validate      :: UserId -> ActionKey -> s -> IO Bool
  stopSession   :: UserId -> ActionKey -> s -> IO ()

{-
  Manages all information in the form of OpenBrain.Data.Information
-}
class InformationBackend b where
  -- | 'Creative' Operations:
  addContentMedia   :: CreateInformation -> Content -> b -> IO InformationId
  addParticipant    :: InformationId -> UserId -> b -> IO () -- | May only target discussions
  createCollection  :: CreateInformation -> [InformationId] -> b -> IO Collection
  createDiscussion  :: CreateInformation -> [InformationId] -> Deadline -> DiscussionType -> b -> IO InformationId
  -- | 'Querying' Operations:
  getInformationCount         :: b -> IO Count
  getInformation              :: InformationId -> b -> IO (Maybe Information)
  getInformations             :: Limit -> Offset -> b -> IO [Information] -- | No parents
  getInformationCountAfter    :: CalendarTime -> b -> IO Count
  getInformationsAfter        :: CalendarTime ->Limit -> Offset -> b -> IO [Information] -- | No parents
  getInformationCountBy       :: UserId -> b -> IO Count
  getInformationBy            :: UserId -> Limit -> Offset -> b -> IO [Information] -- | No parents
  getInformationParentsCount  :: InformationId -> b -> IO Count
  getInformationParents       :: InformationId -> Limit -> Offset -> b -> IO [Information] -- | youngest first
  getProfiledUsers            :: InformationId -> b -> IO [UserData]
  getNextDeadline             :: b -> IO (Maybe Information)
  -- | 'Modifying' Operations:
  updateContentMedia  :: UserId -> InformationId -> Title -> Description -> Content -> b -> IO InformationId
  updateCollection    :: Collection -> [InformationId] -> b -> IO Collection -- | Changes the items of the collection to the given list.
  setParticipant      :: Collection -> UserId -> Bool -> b -> IO ()
  vote                :: InformationId -> UserId -> b -> IO () -- | May only target CollectionType Choice - Discussion is found because it's a parent.
  {-|
    Adds the given lists of InformationId as collections to the system.
    They are than added as choices to the discussion defined by the single InformationId.
    This operation fails if targeted Information is no discussion or already has choices.
  |-}
  setChoices          :: InformationId -> [[InformationId]] -> b -> IO ()
  -- | 'Destructive' Operations:
  deleteInformation :: InformationId -> b -> IO () -- | Sets a delete date on an Information
  removeParticipant :: InformationId -> UserId -> b -> IO () -- | May only target discussions, should not be possible when voted.

class RelationBackend b where
  addRelation     :: Source -> Target -> RelationType -> Comment -> b -> IO RelationId
  deleteRelation  :: RelationId -> b -> IO ()
  getRelation     :: RelationId -> b -> IO (Maybe Relation)
  -- | youngest first, deleted after non deleted
  getRelations    :: InformationId -> RelationEnd -> Maybe RelationType -> AllowDeleted -> b -> IO [Relation]
  updateComment   :: RelationId -> Comment -> b -> IO ()

