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

-- | Functions derived from OpenBrain.Backend classes:

shutdown :: OBB ()
shutdown = withBackend $ \x -> liftIO $ Backend.shutdown x

login :: UserName -> Hash -> OBB UserData
login username hash = withBackend $ \b -> lift $ Backend.login b username hash

getUser :: UserId -> OBB UserData
getUser userid = withBackend $ \b -> lift $ Backend.getUser b userid

hasUserWithId :: UserId -> OBB Bool
hasUserWithId userid = withBackend $ \b -> liftIO $ Backend.hasUserWithId b userid

hasUserWithName :: UserName -> OBB UserId
hasUserWithName username = withBackend $ \b -> lift $ Backend.hasUserWithName b username

register :: UserName -> Hash -> OBB UserData
register username hash = withBackend $ \b -> lift $ Backend.register b username hash

delete :: UserId -> OBB Bool
delete userid = withBackend $ \b -> liftIO $ Backend.delete b userid

getUserCount :: OBB Types.Count
getUserCount = withBackend $ liftIO . Backend.getUserCount

getUserList :: Types.Limit -> Types.Offset -> OBB [UserId]
getUserList limit offset = withBackend $ \b -> liftIO $ Backend.getUserList b limit offset

updateKarma :: UserId -> (Karma -> Karma) -> OBB ()
updateKarma userid f = withBackend $ \b -> liftIO $ Backend.updateKarma b userid f

updatePasswd :: UserId -> Hash -> OBB ()
updatePasswd userid hash = withBackend $ \b -> liftIO $ Backend.updatePasswd b userid hash

setAdmin :: UserId -> Bool -> OBB ()
setAdmin userid isadmin = withBackend $ \b -> liftIO $ Backend.setAdmin b userid isadmin

setProfile :: UserId -> Maybe InformationId -> OBB ()
setProfile uid miid = withBackend $ \b -> liftIO $ Backend.setProfile b uid miid

karmaDeleteUser :: OBB Karma
karmaDeleteUser = withBackend $ liftIO . Backend.karmaDeleteUser

karmaEditUser :: OBB Karma
karmaEditUser = withBackend $ liftIO . Backend.karmaEditUser

setId :: Salt -> UserId -> OBB ()
setId salt userid = withBackend $ \b -> liftIO $ Backend.setId b salt userid

getSalt :: UserId -> OBB Salt
getSalt userid = withBackend $ \b -> liftIO $ Backend.getSalt b userid

removeSalt :: UserId -> OBB ()
removeSalt userid = withBackend $ \b -> liftIO $ Backend.removeSalt b userid

startSession :: UserId -> OBB Types.ActionKey
startSession userid = withBackend $ \b -> liftIO $ Backend.startSession b userid

validate :: UserId -> Types.ActionKey -> OBB Bool
validate userid actionkey = withBackend $ \b -> liftIO $ Backend.validate b userid actionkey

perform :: UserId -> Types.ActionKey -> OBB Types.ActionKey
perform userid actionkey = withBackend $ \b -> lift $ Backend.perform b userid actionkey

stopSession :: UserId -> Types.ActionKey -> OBB ()
stopSession userid actionkey = withBackend $ \b -> liftIO $ Backend.stopSession b userid actionkey

addContentMedia :: Types.CreateInformation -> Types.Content -> OBB InformationId
addContentMedia createinformation content = withBackend $ \b -> liftIO $ Backend.addContentMedia b createinformation content

addToCollection :: Types.Collection -> [InformationId] -> OBB InformationId
addToCollection collection iids = withBackend $ \b -> liftIO $ Backend.addToCollection b collection iids

addParticipant :: InformationId -> UserId -> OBB ()
addParticipant iid uid = withBackend $ \b -> liftIO $ Backend.addParticipant b iid uid

createCollection :: Types.CreateInformation -> [InformationId] -> OBB InformationId
createCollection ci iids = withBackend $ \b -> liftIO $ Backend.createCollection b ci iids

createDiscussion :: Types.CreateInformation -> [InformationId] -> Types.Deadline -> Types.DiscussionType -> OBB InformationId
createDiscussion ci iids dl dt = withBackend $ \b -> liftIO $ Backend.createDiscussion b ci iids dl dt

getInformationCount :: OBB Types.Count
getInformationCount = withBackend $ liftIO . Backend.getInformationCount

getInformation :: InformationId -> OBB Information
getInformation iid = withBackend $ \b -> liftIO $ Backend.getInformation b iid

getInformations :: Types.Limit -> Types.Offset -> OBB [Information]
getInformations limit offset = withBackend $ \b -> liftIO $ Backend.getInformations b limit offset

getInformationsAfter :: Types.Limit -> CalendarTime -> OBB [Information]
getInformationsAfter limit ct = withBackend $ \b -> liftIO $ Backend.getInformationsAfter b limit ct

getInformationCountBy :: UserId -> OBB Types.Count
getInformationCountBy uid = withBackend $ \b -> liftIO $ Backend.getInformationCountBy b uid

getInformationBy :: UserId -> Types.Limit -> Types.Offset -> OBB [Information]
getInformationBy uid limit offset = withBackend $ \b -> liftIO $ Backend.getInformationBy b uid limit offset

getInformationParentsCount :: InformationId -> OBB Types.Count
getInformationParentsCount iid = withBackend $ \b -> liftIO $ Backend.getInformationParentsCount b iid

getInformationParents :: InformationId -> Types.Limit -> Types.Offset -> OBB [Information]
getInformationParents iid limit offset = withBackend $ \b -> liftIO $ Backend.getInformationParents b iid limit offset

getProfiledUsers :: InformationId -> OBB [UserData]
getProfiledUsers iid = withBackend $ \b -> liftIO $ Backend.getProfiledUsers b iid

updateDescription :: InformationId -> Types.Description -> OBB InformationId
updateDescription iid description = withBackend $ \b -> liftIO $ Backend.updateDescription b iid description

updateTitle :: InformationId -> Types.Title -> OBB InformationId
updateTitle iid title = withBackend $ \b -> liftIO $ Backend.updateTitle b iid title

updateContent :: InformationId -> Types.Content -> OBB InformationId
updateContent iid content = withBackend $ \b -> liftIO $ Backend.updateContent b iid content

vote :: InformationId -> UserId -> OBB ()
vote iid uid = withBackend $ \b -> liftIO $ Backend.vote b iid uid

deleteInformation :: InformationId -> OBB ()
deleteInformation iid = withBackend $ \b -> liftIO $ Backend.deleteInformation b iid

removeParticipant :: InformationId -> UserId -> OBB ()
removeParticipant iid uid = withBackend $ \b -> liftIO $ Backend.removeParticipant b iid uid

addRelation :: Types.Source -> Types.Target -> RelationType -> Types.Comment -> OBB ()
addRelation source target rt comment = withBackend $ \b -> liftIO $ Backend.addRelation b source target rt comment

deleteRelation :: RelationId -> OBB ()
deleteRelation rid = withBackend $ \b -> liftIO $ Backend.deleteRelation b rid

getRelations :: InformationId -> OBB [Relation]
getRelations iid = withBackend $ \b -> liftIO $ Backend.getRelations b iid

updateComment :: RelationId -> Types.Comment -> OBB ()
updateComment rid comment = withBackend $ \b -> liftIO $ Backend.updateComment b rid comment

-- | Functions on top of derived ones:
getUserByName :: UserName -> OBB UserData
getUserByName = hasUserWithName >=> getUser

getUsers :: [UserId] -> OBB [UserData]
getUsers = mapM getUser
