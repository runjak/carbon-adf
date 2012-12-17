{-# LANGUAGE Rank2Types #-}
module OpenBrain.Backend.Monad where

import Control.Monad
import Control.Monad.Trans as T
import Control.Monad.Trans.Maybe as M
import Control.Monad.Trans.State as S
import System.Time (CalendarTime)

import OpenBrain.Data.Hash
import OpenBrain.Data.Id
import OpenBrain.Data.Information
import OpenBrain.Data.Karma
import OpenBrain.Data.Relation
import OpenBrain.Data.Salt
import OpenBrain.Data.User
import OpenBrain.Backend (Backend(..))
import qualified OpenBrain.Backend as Backend
import qualified OpenBrain.Backend.Types as Types

-- | The OpenBrain Backend Monad
type OBB a = StateT Backend (MaybeT IO) a

withBackend :: (forall b . ( Backend.GeneralBackend      b
                           , Backend.InformationBackend  b
                           , Backend.KarmaBackend        b
                           , Backend.RelationBackend     b
                           , Backend.SaltShaker          b
                           , Backend.SessionManagement   b
                           , Backend.UserBackend         b
                           ) => b -> OBB a) -> OBB a
withBackend f = do
  (Backend b) <- get
  f b

liftIOM :: IO (Maybe a) -> OBB a
liftIOM = maybe mzero return <=< liftIO

-- | Functions derived from OpenBrain.Backend classes:

shutdown :: OBB ()
shutdown = withBackend $ liftIO . Backend.shutdown

login :: UserName -> Hash -> OBB UserData
login username hash = withBackend $ liftIOM . Backend.login username hash

getUser :: UserId -> OBB UserData
getUser userid = withBackend $ liftIOM . Backend.getUser userid

hasUserWithId :: UserId -> OBB Bool
hasUserWithId userid = withBackend $ liftIO . Backend.hasUserWithId userid

hasUserWithName :: UserName -> OBB UserId
hasUserWithName username = withBackend $ liftIOM . Backend.hasUserWithName username

register :: UserName -> Hash -> Salt -> OBB UserData
register username hash salt = withBackend $ liftIOM . Backend.register username hash salt

delete :: UserId -> Types.Heir -> OBB Bool
delete userid heir = do
  when (userid == heir) mzero
  withBackend $ liftIO . Backend.delete userid heir

getUserCount :: OBB Types.Count
getUserCount = withBackend $ liftIO . Backend.getUserCount

getUserList :: Types.Limit -> Types.Offset -> OBB [UserId]
getUserList limit offset = withBackend $ liftIO . Backend.getUserList limit offset

updateKarma :: UserId -> (Karma -> Karma) -> OBB ()
updateKarma userid f = withBackend $ liftIO . Backend.updateKarma userid f

updatePasswd :: UserId -> Hash -> OBB ()
updatePasswd userid hash = withBackend $ liftIO . Backend.updatePasswd userid hash

setAdmin :: UserId -> Bool -> OBB ()
setAdmin userid isadmin = withBackend $ liftIO . Backend.setAdmin userid isadmin

setProfile :: UserId -> Maybe InformationId -> OBB ()
setProfile uid miid = withBackend $ liftIO . Backend.setProfile uid miid

karmaDeleteUser :: OBB Karma
karmaDeleteUser = withBackend $ liftIO . Backend.karmaDeleteUser

karmaEditUser :: OBB Karma
karmaEditUser = withBackend $ liftIO . Backend.karmaEditUser

getSalt :: UserId -> OBB Salt
getSalt userid = withBackend $ liftIO . Backend.getSalt userid

startSession :: UserId -> OBB Types.ActionKey
startSession userid = withBackend $ liftIO . Backend.startSession userid

validate :: UserId -> Types.ActionKey -> OBB Bool
validate userid actionkey = withBackend $ liftIO . Backend.validate userid actionkey

stopSession :: UserId -> Types.ActionKey -> OBB ()
stopSession userid actionkey = withBackend $ liftIO . Backend.stopSession userid actionkey

addContentMedia :: Types.CreateInformation -> Types.Content -> OBB InformationId
addContentMedia createinformation content = withBackend $ liftIO . Backend.addContentMedia createinformation content

addParticipant :: InformationId -> UserId -> OBB ()
addParticipant iid uid = withBackend $ liftIO . Backend.addParticipant iid uid

createCollection :: Types.CreateInformation -> [InformationId] -> OBB InformationId
createCollection ci iids = withBackend $ liftIO . Backend.createCollection ci iids

createDiscussion :: Types.CreateInformation -> [InformationId] -> Types.Deadline -> Types.DiscussionType -> OBB InformationId
createDiscussion ci iids dl dt = withBackend $ liftIO . Backend.createDiscussion ci iids dl dt

getInformationCount :: OBB Types.Count
getInformationCount = withBackend $ liftIO . Backend.getInformationCount

getInformation :: InformationId -> OBB Information
getInformation iid = withBackend $ liftIOM . Backend.getInformation iid

getInformations :: Types.Limit -> Types.Offset -> OBB [Information]
getInformations limit offset = withBackend $ liftIO . Backend.getInformations limit offset

getInformationCountAfter :: CalendarTime -> OBB Types.Count
getInformationCountAfter ct = withBackend $ liftIO . Backend.getInformationCountAfter ct

getInformationsAfter :: CalendarTime -> Types.Limit -> Types.Offset -> OBB [Information]
getInformationsAfter ct limit offset = withBackend $ liftIO . Backend.getInformationsAfter ct limit offset

getInformationCountBy :: UserId -> OBB Types.Count
getInformationCountBy uid = withBackend $ liftIO . Backend.getInformationCountBy uid

getInformationBy :: UserId -> Types.Limit -> Types.Offset -> OBB [Information]
getInformationBy uid limit offset = withBackend $ liftIO . Backend.getInformationBy uid limit offset

getInformationParentsCount :: InformationId -> OBB Types.Count
getInformationParentsCount iid = withBackend $ liftIO . Backend.getInformationParentsCount iid

getInformationParents :: InformationId -> Types.Limit -> Types.Offset -> OBB [Information]
getInformationParents iid limit offset = withBackend $ liftIO . Backend.getInformationParents iid limit offset

getProfiledUsers :: InformationId -> OBB [UserData]
getProfiledUsers iid = withBackend $ liftIO . Backend.getProfiledUsers iid

getNextDeadline :: OBB (Maybe Information)
getNextDeadline = withBackend $ liftIO . Backend.getNextDeadline

updateContentMedia :: UserId -> InformationId -> Types.Title -> Types.Description -> Types.Content -> OBB InformationId
updateContentMedia uid iid title description content = withBackend $ liftIO . Backend.updateContentMedia uid iid title description content

updateCollection :: Types.Collection -> [InformationId] -> OBB Types.Collection
updateCollection c items = withBackend $ liftIO . Backend.updateCollection c items

setParticipant :: Types.Collection -> UserId -> Bool -> OBB ()
setParticipant c uid status = withBackend $ liftIO . Backend.setParticipant c uid status

vote :: InformationId -> UserId -> OBB ()
vote iid uid = withBackend $ liftIO . Backend.vote iid uid

setChoices :: InformationId -> [[InformationId]] -> OBB ()
setChoices iid choices = withBackend $ liftIO . Backend.setChoices iid choices

deleteInformation :: InformationId -> OBB ()
deleteInformation iid = withBackend $ liftIO . Backend.deleteInformation iid

removeParticipant :: InformationId -> UserId -> OBB ()
removeParticipant iid uid = withBackend $ liftIO . Backend.removeParticipant iid uid

addRelation :: Types.Source -> Types.Target -> RelationType -> Types.Comment -> OBB RelationId
addRelation source target rt comment = withBackend $ liftIO . Backend.addRelation source target rt comment

deleteRelation :: RelationId -> OBB ()
deleteRelation rid = withBackend $ liftIO . Backend.deleteRelation rid

getRelation :: RelationId -> OBB Relation
getRelation rid = withBackend $ liftIOM . Backend.getRelation rid

getRelations :: InformationId -> Types.RelationEnd -> Maybe RelationType -> Types.AllowDeleted -> OBB [Relation]
getRelations iid rEnd mRType aDeleted = withBackend $ liftIO . Backend.getRelations iid rEnd mRType aDeleted

updateComment :: RelationId -> Types.Comment -> OBB ()
updateComment rid comment = withBackend $ liftIO . Backend.updateComment rid comment

-- | Functions on top of derived ones:
getUserByName :: UserName -> OBB UserData
getUserByName = hasUserWithName >=> getUser

getUsers :: [UserId] -> OBB [UserData]
getUsers = mapM getUser

getNobody :: OBB UserId
getNobody = withBackend $ liftIO . Backend.getNobody

delete' :: UserId -> OBB Bool
delete' uid = delete uid =<< getNobody

