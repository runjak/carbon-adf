{-# LANGUAGE DoAndIfThenElse #-}
module OpenBrain.Backend.MysqlBackend.InformationBackend () where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import Database.HDBC as H hiding (clone)
import System.Time (CalendarTime)

import OpenBrain.Backend
import OpenBrain.Backend.MysqlBackend.Convertibles ()
import OpenBrain.Backend.MysqlBackend.Common
import OpenBrain.Backend.MysqlBackend.UserBackend (getUser')
import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Information

import qualified OpenBrain.Data.Relation as R
import qualified OpenBrain.Data.User as U
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
  getProfiledUsers            b = withWConn (conn b) getProfiledUsers'
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

getInformation' :: (IConnection conn) => conn -> InformationId -> MaybeT IO Information
getInformation' conn iid = do
  let selectInformation = "SELECT author, creation, deletion, description, title, mediaid "
                       ++ "FROM Information WHERE informationid = ?"
  rst <- liftIO $ quickQuery conn selectInformation [toSql $ toId iid]
  case rst of
    [[_author, _creation, _deletion, _description, _title, mediaid]] -> do
      user  <- getUser' conn . fromId $ fromSql _author
      media <- liftIO $ getMedia conn iid
      return Information{
        author        = user
      , creation      = fromSql _creation
      , deletion      = fromSql _deletion
      , description   = fromSql _description
      , informationId = iid
      , media         = media
      , title         = fromSql _title
      }
    _ -> mzero

getMedia :: (IConnection conn) => conn -> InformationId -> IO Media
getMedia conn iid = do
  let selectMedia = "SELECT M.mediaid, M.content, M.collectiontype, M.discussionid "
                 ++ "FROM Media AS M JOIN Information AS I USING (mediaid) "
                 ++ "WHERE I.informationid = ?"
      iid'        = toSql $ toId iid
  [[mid, _content, _ctype, did]] <- quickQuery conn selectMedia [iid']
  -- Two constructors for Media:
  if _content /= SqlNull
  then return . Content $ fromSql _content
  else do -- Media is a Collection:
    let ctype = fromSql _ctype :: CollectionType
    -- Fetching arguments:
    let argQuery = "SELECT target FROM Relations WHERE source = ? AND type = ?"
    _args <- quickQuery conn argQuery [iid', toSql R.Collection]
    let args = map (fromId . fromSql . head) _args
    -- Looking for DiscussionInfo:
    dinfo <- runMaybeT $ getDiscussionInfo conn did
    -- Building the complete Collection:
    return $ Collection {
      arguments       = args
    , collectionType  = ctype
    , discussion      = dinfo
    }

type DiscussionId = SqlValue
getDiscussionInfo :: (IConnection conn) => conn -> DiscussionId -> MaybeT IO DiscussionInfo
getDiscussionInfo conn did = do
  guard $ did /= SqlNull
  -- Fetching basic discussion info:
  let q = "SELECT complete, deadline FROM DiscussionInfo WHERE discussionid = ?"
  [[_complete, _deadline]] <- liftIO $ quickQuery conn q [did]
  -- Fetching complete:
  let mkComplete = liftM return . getInformation' conn . fromId $ fromSql _complete
  complete <- (_complete /= SqlNull) ? (mkComplete, mzero)
  -- Fetching the choices:
  let q = "SELECT userid, voted FROM DiscussionParticipants WHERE discussionid = ?"
  _choices  <- liftIO $ quickQuery conn q [did]
  choices   <- mapM (mkChoice conn) _choices
  -- Fetching the participants:
  let q = "SELECT userid, voted FROM DiscussionParticipants WHERE discussionid = ?"
  _parts  <- liftIO $ quickQuery conn q [did]
  parts   <- mapM (mkPart conn) _parts
  -- Building the complete DiscussionInfo:
  return DiscussionInfo {
    choices       = choices
  , complete      = complete
  , deadline      = fromSql _deadline
  , participants  = parts
  }
  where
    mkChoice :: (IConnection conn) => conn -> [SqlValue] -> MaybeT IO (Information, Votes)
    mkChoice conn [_iid, v] = do
      i <- getInformation' conn . fromId $ fromSql _iid
      return (i, fromSql v)
    mkChoice _ _            = mzero
    
    mkPart :: (IConnection conn) => conn -> [SqlValue] -> MaybeT IO (U.UserData, Voted)
    mkPart conn [_uid, v] = do
      u <- getUser' conn . fromId $ fromSql _uid
      return (u, fromSql v)
    mkPart _ _            = mzero

getInformations' :: (IConnection conn) => conn -> Types.Limit -> Types.Offset -> IO [Information]
getInformations' conn limit offset = do
  let q = "SELECT informationid FROM Information ORDER BY creation DESC LIMIT ? OFFSET ?"
  rst <- quickQuery conn q [toSql limit, toSql offset]
  mapM (liftM fromJust . runMaybeT . getInformation' conn . fromId . fromSql . head) rst

getInformationsAfter' :: (IConnection conn) => conn -> CalendarTime -> Types.Limit -> Types.Offset -> IO [Information]
getInformationsAfter' conn ctime limit offset = do
  let q = "SELECT informationid FROM Information WHERE creation > ? ORDER BY creation DESC LIMIT ? OFFSET ?"
  rst <- quickQuery conn q [toSql ctime, toSql limit, toSql offset]
  mapM (liftM fromJust . runMaybeT . getInformation' conn . fromId . fromSql . head) rst

getInformationCountBy' :: (IConnection conn) => conn -> UserId -> IO Types.Count
getInformationCountBy' conn uid = do
  let q = "SELECT COUNT(*) FROM Information WHERE author = ?"
  [[c]] <- quickQuery conn q [toSql $ toId uid]
  return $ fromSql c

getInformationBy' :: (IConnection conn) => conn -> UserId -> Types.Limit -> Types.Offset -> IO [Information]
getInformationBy' conn uid limit offset = do
  let q = "SELECT informationid FROM Information WHERE author = ? ORDER BY creation DESC LIMIT ? OFFSET ?"
  rst <- quickQuery conn q [toSql $ toId uid, toSql limit, toSql offset]
  mapM (liftM fromJust . runMaybeT . getInformation' conn . fromId . fromSql . head) rst

getInformationParentsCount' :: (IConnection conn) => conn -> InformationId -> IO Types.Count
getInformationParentsCount' conn iid = do
  let q = "SELECT COUNT(*) FROM Relations WHERE type = ? AND target = ?"
  [[c]] <- quickQuery conn q [toSql R.Parent, toSql $ toId iid]
  return $ fromSql c

getInformationParents' :: (IConnection conn) => conn -> InformationId -> Types.Limit -> Types.Offset -> IO [Information]
getInformationParents' conn iid limit offset = do
  let q = "SELECT source FROM Relations WHERE target = ? ORDER BY creation DESC LIMIT ? OFFSET ?"
  rst <- quickQuery conn q [toSql $ toId iid, toSql limit, toSql offset]
  mapM (liftM fromJust . runMaybeT . getInformation' conn . fromId . fromSql . head) rst

getProfiledUsers' :: (IConnection conn) => conn -> InformationId -> IO [U.UserData]
getProfiledUsers' conn iid = do
  _uids <- quickQuery conn "SELECT userid FROM UserData WHERE profile = ?" [toSql $ toId iid]
  muds  <- mapM (runMaybeT . getUser' conn . fromId . fromSql . head) _uids
  return $ catMaybes muds

updateDescription' :: (IConnection conn) => conn -> InformationId -> Types.Description -> IO InformationId
updateDescription' conn iid' description = do
  iid <- clone conn iid'
  let q = "UPDATE Information SET description = ? WHERE informationid = ?"
  quickQuery conn q [toSql description, toSql $ toId iid]
  commit conn >> return iid

updateTitle' :: (IConnection conn) => conn -> InformationId -> Types.Title -> IO InformationId
updateTitle' conn iid' title = do
  iid <- clone conn iid'
  let q = "UPDATE Information SET title = ? WHERE informationid = ?"
  quickQuery conn q [toSql title, toSql $ toId iid]
  commit conn >> return iid

updateContent' :: (IConnection conn) => conn -> InformationId -> Types.Content -> IO InformationId
updateContent' conn iid' content = do
  iid <- clone conn iid'
  let q = "UPDATE Media AS M JOIN Information AS I USING (mediaid) SET content = ? WHERE informationid = ?"
  quickQuery conn q [toSql content, toSql $ toId iid]
  commit conn >> return iid

vote' :: (IConnection conn) => conn -> InformationId -> UserId -> IO ()
vote' conn iid uid = do
  let _iid = toSql $ toId iid
      _uid = toSql $ toId uid
  -- Figure out Discussion iid belongs to:
  let getDid = "SELECT discussionid FROM DiscussionChoices WHERE informationid = ?"
  [[did]] <- quickQuery conn getDid [_iid]
  -- Check that UserId belongs to a Participant:
  let pCount = "SELECT COUNT(*) FROM DiscussionParticipants WHERE discussionid = ? AND userid = ?"
  [[c]] <- quickQuery conn pCount [did, _uid]
  when ((fromSql c :: Types.Count) > 0) $ do
    -- Check that Participant hasn't already voted:
    let getVoted = "SELECT voted FROM DiscussionParticipants WHERE discussionid = ? AND userid = ?"
    [[voted]] <- quickQuery conn getVoted [did, _uid]
    when (not $ fromSql voted) $ do
      -- Perform the vote:
      let markVoted   = "UPDATE DiscussionParticipants SET voted = 1 WHERE discussionid = ? AND userid = ?"
          markChoice  = "UPDATE DiscussionChoices SET votes = votes + 1 WHERE discussionid = ? AND informationid = ?"
      quickQuery conn markVoted [did, _uid]
      quickQuery conn markChoice [did, _iid]
      commit conn

deleteInformation' :: (IConnection conn) => conn -> InformationId -> IO ()
deleteInformation' conn iid = do
  let q = "UPDATE Information SET deletion = CURRENT_TIMESTAMP WHERE informationid = ?"
  quickQuery conn q [toSql $ toId iid] >> commit conn

removeParticipant' :: (IConnection conn) => conn -> InformationId -> UserId -> IO ()
removeParticipant' conn iid uid = do
  -- Find the did:
  let getDid = "SELECT discussionid FROM Media JOIN Information USING (mediaid) WHERE informationid = ?"
  [[did]] <- quickQuery conn getDid [toSql $ toId iid]
  -- Check if the deadline has passed:
  let beforeDeadline = "SELECT deadline = NULL FROM DiscussionInfo WHERE discussionid = ?"
  [[validDeadline]] <- quickQuery conn beforeDeadline [did]
  when (fromSql validDeadline) $ do
    -- Check if participant hasn't voted:
    let notVoted = "SELECT voted = 0 FROM DiscussionParticipants WHERE discussionid = ? AND userid = ?"
    [[canVote]] <- quickQuery conn notVoted [did, toSql $ toId uid]
    when (fromSql canVote) $ do
      -- Remove participant:
      let q = "DELETE FROM DiscussionParticipants WHERE discussionid = ? AND userid = ?"
      quickQuery conn q [did, toSql $ toId uid] >> commit conn

