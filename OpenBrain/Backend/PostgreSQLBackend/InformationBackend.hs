{-# LANGUAGE DoAndIfThenElse #-}
module OpenBrain.Backend.PostgreSQLBackend.InformationBackend () where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import Database.HDBC as H hiding (clone)
import System.Time (CalendarTime)

import OpenBrain.Backend
import OpenBrain.Backend.PostgreSQLBackend.Convertibles ()
import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.PostgreSQLBackend.UserBackend (getUser')
import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Information

import qualified OpenBrain.Data.Relation as R
import qualified OpenBrain.Data.User as U
import qualified OpenBrain.Backend.Types as Types

instance InformationBackend PostgreSQLBackend where
  addContentMedia             b = withWConn (conn b) addContentMedia'
  addParticipant              b = withWConn (conn b) addParticipant'
  createCollection            b = withWConn (conn b) createCollection'
  createDiscussion            b = withWConn (conn b) createDiscussion'
  getInformationCount         b = withWConn (conn b) getInformationCount'
  getInformation              b = withWConn (conn b) getInformation'
  getInformations             b = withWConn (conn b) getInformations'
  getInformationCountAfter    b = withWConn (conn b) getInformationCountAfter'
  getInformationsAfter        b = withWConn (conn b) getInformationsAfter'
  getInformationCountBy       b = withWConn (conn b) getInformationCountBy'
  getInformationBy            b = withWConn (conn b) getInformationBy'
  getInformationParentsCount  b = withWConn (conn b) getInformationParentsCount'
  getInformationParents       b = withWConn (conn b) getInformationParents'
  getProfiledUsers            b = withWConn (conn b) getProfiledUsers'
  updateContentMedia          b = withWConn (conn b) updateContentMedia'
  updateCollection            b = withWConn (conn b) updateCollection'
  vote                        b = withWConn (conn b) vote'
  deleteInformation           b = withWConn (conn b) deleteInformation'
  removeParticipant           b = withWConn (conn b) removeParticipant'

{-
  Produces a new Information with the same author, title, description and mediaid
  as the original. Source- and Targetrelations are also cloned.
  Clone is not a transaction on purpose.
-}
clone :: (IConnection conn) => conn -> InformationId -> IO InformationId
clone conn iid = do
  let target = toSql $ toId iid
  -- Creating clone for current target:
  mkChild <- prepare conn $ "INSERT INTO \"Information\" (author, title, description, mediaid) "
                         ++ "SELECT author , title , description , mediaid "
                         ++ "FROM \"Information\" WHERE informationid = ?"
  execute mkChild [target]
  [[clone]] <- quickQuery' conn "SELECT LASTVAL()" []
  -- Copy relations of target to clone
  cSourceRelations <- prepare conn $ "INSERT INTO \"Relations\" (comment, type, source, target) "
                                  ++ "SELECT comment, type, ?, target "
                                  ++ "FROM \"Relations\" WHERE source = ?"
  cTargetRelations <- prepare conn $ "INSERT INTO \"Relations\" (comment, type, source, target) "
                                  ++ "SELECT comment, type, source, ? "
                                  ++ "FROM \"Relations\" WHERE target = ?"
  execute cSourceRelations [clone, target]
  execute cTargetRelations [clone, target]
  -- Mark clone as child of parent:
  mkChild <- prepare conn "INSERT INTO \"Relations\" (comment, type, source, target) VALUES ('', ?, ?, ?)"
  execute mkChild [toSql R.Parent, target, clone]
  -- And it's done:
  return . fromId $ fromSql clone

type MediaId = SqlValue
mkSimpleInformation :: (IConnection conn) => conn -> Types.CreateInformation -> MediaId -> IO InformationId
mkSimpleInformation conn cinfo mediaid = withTransaction conn $ \conn -> do
  iInsert <- prepare conn "INSERT INTO \"Information\" (author, description, title, mediaid) VALUES (?, ?, ?, ?)"
  execute iInsert [toSql . toId $ Types.userId cinfo
                  , toSql $ Types.title cinfo
                  , toSql $ Types.description cinfo
                  , mediaid]
  [[iid]] <- quickQuery' conn "SELECT LASTVAL()" []
  return (fromId $ fromSql iid)

addContentMedia' :: (IConnection conn) => conn -> Types.CreateInformation -> Types.Content -> IO InformationId
addContentMedia' conn cinfo content = withTransaction conn $ \conn -> do
  mInsert <- prepare conn "INSERT INTO \"Media\" (content) VALUES (?)"
  execute mInsert [toSql content]
  [[mediaId]] <- quickQuery' conn "SELECT LASTVAL()" []
  iInsert <- prepare conn "INSERT INTO \"Information\" (author, title, description, mediaid) VALUES (?, ?, ?, ?)"
  execute iInsert [toSql . toId $ Types.userId cinfo
                  , toSql $ Types.title cinfo
                  , toSql $ Types.description cinfo
                  , mediaId]
  [[iid]] <- quickQuery' conn "SELECT LASTVAL()" []
  return . fromId $ fromSql iid

addParticipant' :: (IConnection conn) => conn -> InformationId -> UserId -> IO ()
addParticipant' conn iid uid = withTransaction conn $ \conn -> do
  let fetchDiscussion = "SELECT discussionid FROM \"DiscussionInfo\" AS D "
                      ++ "JOIN \"Media\" USING (discussionid) "
                      ++ "JOIN \"Information\" AS I USING (mediaid) "
                      ++ "WHERE D.deadline >= CURRENT_TIMESTAMP "
                      ++ "AND I.informationid = ?"
  rst <- quickQuery' conn fetchDiscussion [toSql $ toId iid]
  unless (null rst) $ do
    let did = head $ head rst
    stmt <- prepare conn "INSERT INTO \"DiscussionParticipants\" (discussionid, userid) VALUES (?, ?)"
    void $ execute stmt [did, toSql $ toId uid]

createCollection' :: (IConnection conn) => conn -> Types.CreateInformation -> [InformationId] -> IO Types.Collection
createCollection' conn cinfo elems = withTransaction conn $ \conn -> do
  -- Insert a new media for the collection
  quickQuery' conn "INSERT INTO \"Media\" (collectiontype) VALUES (?)" [toSql SimpleCollection]
  [[mediaid]] <- quickQuery' conn "SELECT LASTVAL()" []
  -- Insert a new information
  iid <- mkSimpleInformation conn cinfo mediaid
  -- Insert relations
  collect <- prepare conn "INSERT INTO \"Relations\" (comment, type, source, target) VALUES ('', ?, ?, ?)"
  executeMany collect [[toSql R.Collection, toSql $ toId iid, toSql $ toId e] | e <- elems]
  return iid

createDiscussion' :: (IConnection conn) => conn -> Types.CreateInformation -> [InformationId] -> Types.Deadline -> Types.DiscussionType -> IO InformationId
createDiscussion' conn cinfo arguments deadline dtype = withTransaction conn $ \conn -> do
  -- Producing a new discussion:
  quickQuery' conn "INSERT INTO \"DiscussionInfo\" (deadline) VALUES (?)" [toSql deadline]
  [[did]] <- quickQuery' conn "SELECT LASTVAL()" []
  -- Linking arguments to discussion:
  mkArg <- prepare conn "INSERT INTO \"DiscussionChoices\" (discussionid, informationid) VALUES (?, ?)"
  executeMany mkArg [[did, toSql $ toId a] | a <- arguments]
  -- Produce a media for the discussion:
  mkMedia <- prepare conn "INSERT INTO \"Media\" (collectiontype, discussionid) VALUES (?, ?)"
  execute mkMedia [toSql $ Types.toCollectionType dtype, did]
  [[mediaid]] <- quickQuery' conn "SELECT LASTVAL()" []
  -- Produce Information for the Media:
  mkSimpleInformation conn cinfo mediaid

getInformationCount' :: (IConnection conn) => conn -> IO Types.Count
getInformationCount' conn = do
  [[c]] <- quickQuery' conn "SELECT COUNT(*) FROM \"Information\" WHERE deletion IS NULL" []  
  return $ fromSql c

getInformation' :: (IConnection conn) => conn -> InformationId -> MaybeT IO Information
getInformation' conn iid = do
  let selectInformation = "SELECT author, creation, deletion, description, title, mediaid "
                       ++ "FROM \"Information\" WHERE informationid = ?"
  rst <- liftIO $ quickQuery' conn selectInformation [toSql $ toId iid]
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
                 ++ "FROM \"Media\" AS M JOIN \"Information\" AS I USING (mediaid) "
                 ++ "WHERE I.informationid = ?"
      iid'        = toSql $ toId iid
  [[mid, _content, _ctype, did]] <- quickQuery' conn selectMedia [iid']
  -- Two constructors for Media:
  if _content /= SqlNull
    then return . Content $ fromSql _content
    else do -- Media is a Collection:
      let ctype = fromSql _ctype :: CollectionType
      -- Fetching arguments:
      let argQuery = "SELECT target FROM \"Relations\" WHERE source = ? AND type = ?"
      _args <- quickQuery' conn argQuery [iid', toSql R.Collection]
      let args = map (fromId . fromSql . head) _args
      -- Looking for DiscussionInfo:
      dinfo <- runMaybeT $ getDiscussionInfo conn did
      -- Building the complete Collection:
      return Collection {
        arguments       = args
      , collectionType  = ctype
      , discussion      = dinfo
      }

type DiscussionId = SqlValue
getDiscussionInfo :: (IConnection conn) => conn -> DiscussionId -> MaybeT IO DiscussionInfo
getDiscussionInfo conn did = do
  guard $ did /= SqlNull
  -- Fetching basic discussion info:
  let q = "SELECT complete, deadline FROM \"DiscussionInfo\" WHERE discussionid = ?"
  [[_complete, _deadline]] <- liftIO $ quickQuery' conn q [did]
  -- Fetching complete:
  let mkComplete = liftM return . getInformation' conn . fromId $ fromSql _complete
  complete <- (_complete /= SqlNull) ? (mkComplete, mzero)
  -- Fetching the choices:
  let q = "SELECT userid, voted FROM \"DiscussionParticipants\" WHERE discussionid = ?"
  _choices  <- liftIO $ quickQuery' conn q [did]
  choices   <- mapM (mkChoice conn) _choices
  -- Fetching the participants:
  let q = "SELECT userid, voted FROM \"DiscussionParticipants\" WHERE discussionid = ?"
  _parts  <- liftIO $ quickQuery' conn q [did]
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
  let q = "SELECT informationid FROM \"Information\" WHERE deletion IS NULL ORDER BY creation DESC LIMIT ? OFFSET ?"
  rst <- quickQuery' conn q [toSql limit, toSql offset]
  mapM (liftM fromJust . runMaybeT . getInformation' conn . fromId . fromSql . head) rst

getInformationCountAfter' :: (IConnection conn) => conn -> CalendarTime -> IO Types.Count
getInformationCountAfter' conn ctime = do
  let q = "SELECT COUNT(*) FROM \"Information\" WHERE deletion IS NULL AND creation > ?"
  [[c]] <- quickQuery' conn q [toSql ctime]
  return $ fromSql c

getInformationsAfter' :: (IConnection conn) => conn -> CalendarTime -> Types.Limit -> Types.Offset -> IO [Information]
getInformationsAfter' conn ctime limit offset = do
  let q = "SELECT informationid FROM \"Information\" WHERE deletion IS NULL AND creation > ? ORDER BY creation DESC LIMIT ? OFFSET ?"
  rst <- quickQuery' conn q [toSql ctime, toSql limit, toSql offset]
  mapM (liftM fromJust . runMaybeT . getInformation' conn . fromId . fromSql . head) rst

getInformationCountBy' :: (IConnection conn) => conn -> UserId -> IO Types.Count
getInformationCountBy' conn uid = do
  let q = "SELECT COUNT(*) FROM \"Information\" WHERE author = ? AND deletion IS NULL"
  [[c]] <- quickQuery' conn q [toSql $ toId uid]
  return $ fromSql c

getInformationBy' :: (IConnection conn) => conn -> UserId -> Types.Limit -> Types.Offset -> IO [Information]
getInformationBy' conn uid limit offset = do
  let q = "SELECT informationid FROM \"Information\" WHERE author = ? AND deletion IS NULL ORDER BY creation DESC LIMIT ? OFFSET ?"
  rst <- quickQuery' conn q [toSql $ toId uid, toSql limit, toSql offset]
  mapM (liftM fromJust . runMaybeT . getInformation' conn . fromId . fromSql . head) rst

getInformationParentsCount' :: (IConnection conn) => conn -> InformationId -> IO Types.Count
getInformationParentsCount' conn iid = do
  let q = "SELECT COUNT(*) FROM \"Relations\" WHERE type = ? AND target = ?"
  [[c]] <- quickQuery' conn q [toSql R.Parent, toSql $ toId iid]
  return $ fromSql c

getInformationParents' :: (IConnection conn) => conn -> InformationId -> Types.Limit -> Types.Offset -> IO [Information]
getInformationParents' conn iid limit offset = do
  let q = "SELECT source FROM \"Relations\" WHERE target = ? ORDER BY creation DESC LIMIT ? OFFSET ?"
  rst <- quickQuery' conn q [toSql $ toId iid, toSql limit, toSql offset]
  mapM (liftM fromJust . runMaybeT . getInformation' conn . fromId . fromSql . head) rst

getProfiledUsers' :: (IConnection conn) => conn -> InformationId -> IO [U.UserData]
getProfiledUsers' conn iid = do
  _uids <- quickQuery' conn "SELECT userid FROM \"UserData\" WHERE profile = ?" [toSql $ toId iid]
  muds  <- mapM (runMaybeT . getUser' conn . fromId . fromSql . head) _uids
  return $ catMaybes muds

updateContentMedia' :: (IConnection conn) => conn -> UserId -> InformationId -> Types.Title -> Types.Description -> Types.Content -> IO InformationId
updateContentMedia' conn uid iid' title description content = withTransaction conn $ \conn -> do
  iid <- clone conn iid'
  let updateInformation = "UPDATE \"Information\" SET title = ?, description = ?, author = ? WHERE informationid = ?"
  quickQuery' conn updateInformation [toSql title, toSql description, toSql $ toId uid, toSql $ toId iid]
  let lookupOldContent = "SELECT content FROM \"Media\" JOIN \"Information\" USING (mediaid) WHERE informationid = ?"
  cRst <- quickQuery' conn lookupOldContent [toSql $ toId iid]
  let cChanged = case cRst of
                    [[content']] -> content /= fromSql content'
                    _ -> False
  when cChanged $ do
    insertMedia <- prepare conn "INSERT INTO \"Media\" (content) VALUES (?)"
    execute insertMedia [toSql content]
    [[mid]] <- quickQuery' conn "SELECT LASTVAL()" []
    quickQuery' conn "UPDATE \"Information\" SET mediaid = ? WHERE informationid = ?" [mid, toSql $ toId iid]
    return ()
  return iid

updateCollection' :: (IConnection conn) => conn -> Types.Collection -> [InformationId] -> IO Types.Collection
updateCollection' conn c items = withTransaction conn $ \conn -> do
  -- We clone the collection to modify it
  c' <- clone conn c
  -- We delete the current items from c'
  quickQuery' conn "DELETE FROM \"Relations\" WHERE type = ? AND source = ?" [toSql R.Collection, toSql $ toId c']
  -- We add the new items to c'
  stmt <- prepare conn "INSERT INTO \"Relations\" (comment, type, source, target) VALUES ('', ?, ?, ?)"
  executeMany stmt [[toSql R.Collection, toSql $ toId c', toSql $ toId i] | i <- items]
  -- Done
  return c'

vote' :: (IConnection conn) => conn -> InformationId -> UserId -> IO ()
vote' conn iid uid = withTransaction conn $ \conn -> do
  let _iid = toSql $ toId iid
      _uid = toSql $ toId uid
  -- Figure out Discussion iid belongs to:
  let getDid = "SELECT discussionid FROM \"DiscussionChoices\" WHERE informationid = ?"
  [[did]] <- quickQuery' conn getDid [_iid]
  -- Check that UserId belongs to a Participant:
  let pCount = "SELECT COUNT(*) FROM \"DiscussionParticipants\" WHERE discussionid = ? AND userid = ?"
  [[c]] <- quickQuery' conn pCount [did, _uid]
  when ((fromSql c :: Types.Count) > 0) $ do
    -- Check that Participant hasn't already voted:
    let getVoted = "SELECT voted FROM \"DiscussionParticipants\" WHERE discussionid = ? AND userid = ?"
    [[voted]] <- quickQuery' conn getVoted [did, _uid]
    unless (fromSql voted) $ do
      -- Perform the vote:
      let markVoted   = "UPDATE \"DiscussionParticipants\" SET voted = 1 WHERE discussionid = ? AND userid = ?"
          markChoice  = "UPDATE \"DiscussionChoices\" SET votes = votes + 1 WHERE discussionid = ? AND informationid = ?"
      quickQuery' conn markVoted [did, _uid]
      quickQuery' conn markChoice [did, _iid]
      return ()

deleteInformation' :: (IConnection conn) => conn -> InformationId -> IO ()
deleteInformation' conn iid = withTransaction conn $ \conn -> do
  let q = "UPDATE \"Information\" SET deletion = CURRENT_TIMESTAMP WHERE informationid = ?"
  void $ quickQuery' conn q [toSql $ toId iid]

removeParticipant' :: (IConnection conn) => conn -> InformationId -> UserId -> IO ()
removeParticipant' conn iid uid = withTransaction conn $ \conn -> do
  -- Find the did:
  let getDid = "SELECT discussionid FROM \"Media\" JOIN \"Information\" USING (mediaid) WHERE informationid = ?"
  [[did]] <- quickQuery' conn getDid [toSql $ toId iid]
  -- Check if the deadline has passed:
  let beforeDeadline = "SELECT deadline = NULL FROM \"DiscussionInfo\" WHERE discussionid = ?"
  [[validDeadline]] <- quickQuery' conn beforeDeadline [did]
  when (fromSql validDeadline) $ do
    -- Check if participant hasn't voted:
    let notVoted = "SELECT voted = 0 FROM \"DiscussionParticipants\" WHERE discussionid = ? AND userid = ?"
    [[canVote]] <- quickQuery' conn notVoted [did, toSql $ toId uid]
    when (fromSql canVote) $ do
      -- Remove participant:
      let q = "DELETE FROM \"DiscussionParticipants\" WHERE discussionid = ? AND userid = ?"
      void $ quickQuery' conn q [did, toSql $ toId uid]

