module OpenBrain.Backend.MysqlBackend.InformationBackend () where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import Database.HDBC as H hiding (clone)
import System.Time (CalendarTime)

import OpenBrain.Data.Id
import OpenBrain.Data.Information
import OpenBrain.Backend
import OpenBrain.Backend.MysqlBackend.Convertibles ()
import OpenBrain.Backend.MysqlBackend.Common
import OpenBrain.Backend.MysqlBackend.UserBackend (getUser')

import qualified OpenBrain.Data.Relation as R
import qualified OpenBrain.Backend.Types as Types

instance InformationBackend MysqlBackend where
  addContentMedia             b = withWConn (conn b) addContentMedia'
  addToCollection             b = withWConn (conn b) addToCollection'
  addParticipant              b = withWConn (conn b) addParticipant'
  createCollection            b = withWConn (conn b) createCollection'
  createDiscussion            b = withWConn (conn b) createDiscussion'
  getInformationCount         b = withWConn (conn b) getInformationCount'
  getInformation              b = withWConn (conn b) getInformation'
  getInformations             b = withWConn (conn b) getInformations'
  getInformationsAfter        b = withWConn (conn b) getInformationsAfter'
  getInformationCountBy       b = withWConn (conn b) getInformationCountBy'
  getInformationBy            b = withWConn (conn b) getInformationBy'
  getInformationParentsCount  b = withWConn (conn b) getInformationParentsCount'
  getInformationParents       b = withWConn (conn b) getInformationParents'
  updateDescription           b = withWConn (conn b) updateDescription'
  updateTitle                 b = withWConn (conn b) updateTitle'
  updateContent               b = withWConn (conn b) updateContent'
  vote                        b = withWConn (conn b) vote'
  deleteInformation           b = withWConn (conn b) deleteInformation'
  removeParticipant           b = withWConn (conn b) removeParticipant'

{-
  Produces a new Information with the same author, title, description and mediaid
  as the original. Source- and Targetrelations are also cloned.
-}
clone :: (IConnection conn) => conn -> InformationId -> IO InformationId
clone conn iid = do
  let target = toSql $ toId iid
  -- Creating clone for current target:
  mkChild <- prepare conn $ "INSERT INTO Information (author, title, description, mediaid) "
                         ++ "SELECT author , title , description , mediaid "
                         ++ "FROM Information WHERE informationid = ?"
  execute mkChild [target]
  [[clone]] <- quickQuery conn "SELECT LAST_INSERT_ID()" []
  -- Mark clone as child of parent:
  mkChild <- prepare conn "INSERT INTO Relations(comment, type, source, target) VALUES ('', ?, ?, ?)"
  execute mkChild [toSql R.Parent, target, clone]
  -- Copy relations of target to clone
  cSourceRelations <- prepare conn $ "INSERT INTO Relations (comment, type, source, target) "
                                  ++ "SELECT 'Copy from parent', type, ?, target "
                                  ++ "FROM Relations WHERE source = ?"
  cTargetRelations <- prepare conn $ "INSERT INTO Relations (comment, type, source, target) "
                                  ++ "SELECT 'Copy from parent', type, source, ? "
                                  ++ "FROM Relations WHERE target = ?"
  execute cSourceRelations [clone, target]
  execute cTargetRelations [clone, target]
  -- And it's done:
  return . fromId $ fromSql clone

type MediaId = SqlValue
mkSimpleInformation :: (IConnection conn) => conn -> Types.CreateInformation -> MediaId -> IO InformationId
mkSimpleInformation conn cinfo mediaid = do
  iInsert <- prepare conn "INSERT INTO Information(author, description, title, mediaid) VALUES (?, ?, ?, ?)"
  execute iInsert [toSql . toId $ Types.userId cinfo
                  , toSql $ Types.title cinfo
                  , toSql $ Types.description cinfo
                  , mediaid]
  [[iid]] <- quickQuery conn "SELECT LAST_INSERT_ID()" []
  commit conn >> return (fromId $ fromSql iid)

addContentMedia' :: (IConnection conn) => conn -> Types.CreateInformation -> Types.Content -> IO InformationId
addContentMedia' conn cinfo content = do
  mInsert <- prepare conn "INSERT INTO Media(content) VALUES (?)"
  execute mInsert [toSql content]
  [[mediaId]] <- quickQuery conn "SELECT LAST_INSERT_ID()" []
  iInsert <- prepare conn "INSERT INTO Information(author, description, title, mediaid) VALUES (?, ?, ?, ?)"
  execute iInsert [toSql . toId $ Types.userId cinfo
                  , toSql $ Types.title cinfo
                  , toSql $ Types.description cinfo
                  , mediaId]
  [[iid]] <- quickQuery conn "SELECT LAST_INSERT_ID()" []
  commit conn
  return . fromId $ fromSql iid

addToCollection' :: (IConnection conn) => conn -> Types.Collection -> [InformationId] -> IO InformationId
addToCollection' conn c elems = do
  -- We clone the collection to modify it
  c' <- clone conn c
  -- Add elems to belong to c'
  stmt <- prepare conn "INSERT INTO Relations(comment, type, source, target) VALUES ('', ?, ?, ?)"
  executeMany stmt [[toSql R.Collection, toSql $ toId c', toSql $ toId e] | e <- elems]
  -- Finish
  commit conn
  return c'

addParticipant' :: (IConnection conn) => conn -> InformationId -> UserId -> IO ()
addParticipant' conn iid uid = do
  let fetchDiscussion = "SELECT discussionid FROM DiscussionInfo AS D "
                      ++ "JOIN Media USING (discussionid) "
                      ++ "JOIN Information AS I USING (mediaid) "
                      ++ "WHERE D.deadline >= CURRENT_TIMESTAMP "
                      ++ "AND I.informationid = ?"
  rst <- quickQuery conn fetchDiscussion [toSql $ toId iid]
  when (not $ null rst) $ do
    let did = head $ head rst
    stmt <- prepare conn "INSERT INTO DiscussionParticipants(discussionid, userid) VALUES (?, ?)"
    execute stmt [did, toSql $ toId uid] >> commit conn

createCollection' :: (IConnection conn) => conn -> Types.CreateInformation -> [InformationId] -> IO Types.Collection
createCollection' conn cinfo elems = do
  -- Insert a new media for the collection
  quickQuery conn "INSERT INTO Media (collectiontype) VALUES (?)" [toSql SimpleCollection]
  [[mediaid]] <- quickQuery conn "SELECT LAST_INSERT_ID()" []
  -- Insert a new information
  iid <- mkSimpleInformation conn cinfo mediaid
  -- Insert relations
  collect <- prepare conn "INSERT INTO Relations (comment, type, source, target) VALUES ('', ?, ?, ?)"
  executeMany collect [[toSql R.Collection, toSql $ toId iid, toSql $ toId e] | e <- elems]
  commit conn >> return iid

createDiscussion' :: (IConnection conn) => conn -> Types.CreateInformation -> [InformationId] -> Types.Deadline -> Types.DiscussionType -> IO InformationId
createDiscussion' conn cinfo arguments deadline dtype = do
  -- Producing a new discussion:
  quickQuery conn "INSERT INTO DiscussionInfo (deadline) VALUES (?)" [toSql deadline]
  [[did]] <- quickQuery conn "SELECT LAST_INSERT_ID()" []
  -- Linking arguments to discussion:
  mkArg <- prepare conn "INSERT INTO DiscussionChoices (discussionid, informationid) VALUES (?, ?)"
  executeMany mkArg [[did, toSql $ toId a] | a <- arguments]
  -- Produce a media for the discussion:
  mkMedia <- prepare conn "INSERT INTO Media (collectiontype, discussionid) VALUES (?, ?)"
  execute mkMedia [toSql $ Types.toCollectionType dtype, did]
  [[mediaid]] <- quickQuery conn "SELECT LAST_INSERT_ID()" []
  -- Produce Information for the Media:
  mkSimpleInformation conn cinfo mediaid

getInformationCount' :: (IConnection conn) => conn -> IO Types.Count
getInformationCount' conn = do
  [[c]] <- quickQuery conn "SELECT COUNT(*) FROM Information" []  
  return $ fromSql c

getInformation' :: (IConnection conn) => conn -> InformationId -> IO Information
getInformation' conn iid = do
  let selectInformation = "SELECT author, creation, deletion, description, title, mediaid "
                       ++ "FROM Information WHERE informationid = ?"
  [[_author, _creation, _deletion, _description, _title, mediaid]] <- quickQuery conn selectInformation [toSql $ toId iid]
  user  <- liftM fromJust . runMaybeT . getUser' conn . fromId $ fromSql _author
  media <- getMedia conn mediaid
  return Information{
    author        = user
  , creation      = fromSql _creation
  , deletion      = fromSql _deletion
  , description   = fromSql _description
  , informationId = iid
  , media         = media
  , title         = fromSql _title
  }

getMedia :: (IConnection conn) => conn -> MediaId -> IO Media
getMedia conn mediaid = do
  let selectMedia = "SELECT content, collectiontype, discussionid FROM Media WHERE mediaid = ?"
  [_content, _ctype, did] -- FIXME WIP
  undefined

getInformations' :: (IConnection conn) => conn -> Types.Limit -> Types.Offset -> IO [Information]
getInformations' conn limit offset = undefined

getInformationsAfter' :: (IConnection conn) => conn -> Types.Limit -> CalendarTime -> IO [Information]
getInformationsAfter' conn limit ctime = undefined

getInformationCountBy' :: (IConnection conn) => conn -> UserId -> IO Types.Count
getInformationCountBy' conn uid = undefined

getInformationBy' :: (IConnection conn) => conn -> UserId -> Types.Limit -> Types.Offset -> IO [Information]
getInformationBy' conn uid limit offset = undefined

getInformationParentsCount' :: (IConnection conn) => conn -> InformationId -> IO Types.Count
getInformationParentsCount' conn iid = undefined

getInformationParents' :: (IConnection conn) => conn -> Types.Limit -> Types.Offset -> IO [Information]
getInformationParents' conn limit offset = undefined

updateDescription' :: (IConnection conn) => conn -> InformationId -> Types.Description -> IO InformationId
updateDescription' conn iid description = undefined

updateTitle' :: (IConnection conn) => conn -> InformationId -> Types.Title -> IO InformationId
updateTitle' conn idd title = undefined

updateContent' :: (IConnection conn) => conn -> InformationId -> Types.Content -> IO InformationId
updateContent' conn iid content = undefined

vote' :: (IConnection conn) => conn -> InformationId -> UserId -> IO ()
vote' conn iid uid = undefined

deleteInformation' :: (IConnection conn) => conn -> InformationId -> IO ()
deleteInformation' conn iid = undefined

removeParticipant' :: (IConnection conn) => conn -> InformationId -> UserId -> IO ()
removeParticipant' conn iid uid = undefined

