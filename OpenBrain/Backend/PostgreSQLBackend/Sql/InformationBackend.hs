module OpenBrain.Backend.PostgreSQLBackend.Sql.InformationBackend where

import Data.Maybe
import System.Time (CalendarTime)

import OpenBrain.Backend.PostgreSQLBackend.Common hiding (clone)
import OpenBrain.Backend.PostgreSQLBackend.Sql.InformationBackend.Helper
import OpenBrain.Backend.PostgreSQLBackend.Sql.InformationBackend.Get as IGet
import OpenBrain.Backend.PostgreSQLBackend.Sql.UserBackend (getUser', getNobody')
import OpenBrain.Data

type MediaId = SqlValue
mkSimpleInformation :: (IConnection conn) => CreateInformation -> MediaId -> conn -> IO InformationId
mkSimpleInformation cinfo mediaid conn = do
  iInsert <- prepare conn "INSERT INTO \"Information\" (author, title, description, mediaid) VALUES (?, ?, ?, ?)"
  execute iInsert [toSql . toId $ userId cinfo
                  , toSql $ title cinfo
                  , toSql $ description cinfo
                  , mediaid]
  [[iid]] <- quickQuery' conn "SELECT LASTVAL()" []
  return (fromId $ fromSql iid)

addContentMedia' :: (IConnection conn) => CreateInformation -> Content -> conn -> IO InformationId
addContentMedia' cinfo content conn = do
  mInsert <- prepare conn "INSERT INTO \"Media\" (content) VALUES (?)"
  execute mInsert [toSql content]
  [[mediaId]] <- quickQuery' conn "SELECT LASTVAL()" []
  iInsert <- prepare conn "INSERT INTO \"Information\" (author, title, description, mediaid) VALUES (?, ?, ?, ?)"
  execute iInsert [toSql . toId $ userId cinfo
                  , toSql $ title cinfo
                  , toSql $ description cinfo
                  , mediaId]
  [[iid]] <- quickQuery' conn "SELECT LASTVAL()" []
  return . fromId $ fromSql iid

addParticipant' :: (IConnection conn) => InformationId -> UserId -> conn -> IO ()
addParticipant' iid uid conn = do
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

createCollection' :: (IConnection conn) => CreateInformation -> [InformationId] -> conn -> IO Collection
createCollection' cinfo elems conn = do
  -- Insert a new media for the collection
  quickQuery' conn "INSERT INTO \"Media\" (collectiontype) VALUES (?)" [toSql SimpleCollection]
  [[mediaid]] <- quickQuery' conn "SELECT LASTVAL()" []
  -- Insert a new information
  iid <- mkSimpleInformation cinfo mediaid conn
  -- Insert relations
  collect <- prepare conn "INSERT INTO \"Relations\" (comment, type, source, target) VALUES ('', ?, ?, ?)"
  executeMany collect [[toSql Collection, toSql $ toId iid, toSql $ toId e] | e <- elems]
  return iid

createDiscussion' :: (IConnection conn) => CreateInformation
                                        -> [InformationId] -> Deadline
                                        -> DiscussionType -> conn
                                        -> IO InformationId
createDiscussion' cinfo arguments deadline dtype conn = do
  -- Producing a new discussion:
  quickQuery' conn "INSERT INTO \"DiscussionInfo\" (deadline) VALUES (?)" [toSql deadline]
  [[did]] <- quickQuery' conn "SELECT LASTVAL()" []
  -- Produce a media for the discussion:
  mkMedia <- prepare conn "INSERT INTO \"Media\" (collectiontype, discussionid) VALUES (?, ?)"
  execute mkMedia [toSql $ toCollectionType dtype, did]
  [[mediaid]] <- quickQuery' conn "SELECT LASTVAL()" []
  -- Produce Information for the Media:
  iid <- mkSimpleInformation cinfo mediaid conn
  -- Linking arguments to discussion:
  mkArgs <- prepare conn "INSERT INTO \"Relations\" (comment, type, source, target) VALUES ('', ?, ?, ?)"
  executeMany mkArgs [[toSql Collection, toSql $ toId iid, toSql $ toId a] | a <- arguments]
  return iid

updateContentMedia' :: (IConnection conn) => UserId -> InformationId
                                          -> Title -> Description
                                          -> Content -> conn
                                          -> IO InformationId
updateContentMedia' uid iid' title description content conn = do
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

updateCollection' :: (IConnection conn) => Collection -> [InformationId] -> conn -> IO Collection
updateCollection' c items conn = do
  -- We clone the collection to modify it
  c' <- clone conn c
  -- We delete the current items from c'
  quickQuery' conn "DELETE FROM \"Relations\" WHERE type = ? AND source = ?" [toSql Collection, toSql $ toId c']
  -- We add the new items to c'
  stmt <- prepare conn "INSERT INTO \"Relations\" (comment, type, source, target) VALUES ('', ?, ?, ?)"
  executeMany stmt [[toSql Collection, toSql $ toId c', toSql $ toId i] | i <- items]
  -- Done
  return c'

setParticipant' :: (IConnection conn) => Collection -> UserId -> Bool -> conn -> IO ()
setParticipant' c uid status conn = do
  -- Fetching the discussionId:
  let getDid = "SELECT discussionid FROM \"Media\" JOIN \"Information\" USING (mediaid) WHERE informationid = ?"
  [[did]] <- quickQuery' conn getDid [toSql $ toId c]
  -- Fetching the participant:
  let getP = "SELECT voted, userid FROM \"DiscussionParticipants\" WHERE discussionid = ? AND userid = ?"
  rst <- quickQuery' conn getP [did, toSql $ toId uid]
  case rst of
    [[voted, uid']] -> do -- Removing a participant
      when status . error $
        "User " ++ show uid ++ " is alread a participant of discussion " ++ show c ++ " so it cannot be added again."
      when (fromSql voted) . error $
        "User " ++ show uid ++ " must stay a participant of discussion " ++ show c ++ " because it already voted."
      stmt <- prepare conn "DELETE FROM \"DiscussionParticipants\" WHERE discussionid = ? AND userid = ?"  
      void $ execute stmt [did, uid']
    _ -> do -- Adding a participant
      unless status . error $
        "User " ++ show uid ++ " is not a participant of discussion " ++ show c ++ " so it can't be removed."
      stmt <- prepare conn "INSERT INTO \"DiscussionParticipants\" (discussionid, voted, userid) VALUES (?, ?, ?)"
      void $ execute stmt [did, toSql False, toSql $ toId uid]

vote' :: (IConnection conn) => InformationId -> UserId -> conn -> IO ()
vote' iid uid conn = do
  let _iid = toSql $ toId iid
      _uid = toSql $ toId uid
  -- Figure out Discussion iid belongs to:
  let getDid = "SELECT discussionid FROM \"DiscussionChoices\" WHERE informationid = ?"
  [[did]] <- quickQuery' conn getDid [_iid]
  -- Check that UserId belongs to a Participant:
  let pCount = "SELECT COUNT(*) FROM \"DiscussionParticipants\" WHERE discussionid = ? AND userid = ?"
  [[c]] <- quickQuery' conn pCount [did, _uid]
  unless ((fromSql c :: Count) > 0) . error $
    "User " ++ show uid ++ " is not a participant of Discussion " ++ show iid
  -- Check that Participant hasn't already voted:
  let getVoted = "SELECT voted FROM \"DiscussionParticipants\" WHERE discussionid = ? AND userid = ?"
  [[voted]] <- quickQuery' conn getVoted [did, _uid]
  when (fromSql voted) . error $
    "User " ++ show uid ++ " already voted on Discussion " ++ show iid
  -- Perform the vote:
  let markVoted   = "UPDATE \"DiscussionParticipants\" SET voted = 1 WHERE discussionid = ? AND userid = ?"
      markChoice  = "UPDATE \"DiscussionChoices\" SET votes = votes + 1 WHERE discussionid = ? AND informationid = ?"
  quickQuery' conn markVoted [did, _uid]
  quickQuery' conn markChoice [did, _iid]
  return ()

setChoices' :: (IConnection conn) => InformationId -> [[InformationId]] -> conn -> IO ()
setChoices' iid choices conn = do
  -- | Fetching the discussionid for the given iid:
  let getDid = "SELECT discussionid FROM \"Media\" JOIN \"Information\" USING (mediaid) WHERE informationid = ?"
  rst <- quickQuery' conn getDid [toSql $ toId iid]
  let did = case rst of
              [[did]] -> did
              _       -> error $ "Could not fetch discussionid for iid: "
                      ++ show iid ++ " in OpenBrain.Backend.PostgreSqlBackend:setChoices'"
  -- | Checking that choices havn't been set:
  let getChoiceCount = "SELECT COUNT(*) FROM \"DiscussionChoices\" WHERE discussionid = ?"
  [[count]] <- quickQuery' conn getChoiceCount [did]
  when ((fromSql count :: Int) > 0) $
    error $ "Choices already set for InformationId: "
    ++ show iid ++ " in OpenBrain.Backend.PostgreSqlBackend:setChoices'"
  -- | Create choices as Information:
  nobody <- getNobody' conn
  let ci = CreateInformation {
      userId        = nobody
    , ciTitle       = "Autogenerated choice"
    , ciDescription = "A choice to a discussion that was generated by OpenBrain."
    }
  cIds <- mapM (\c -> createCollection' ci c conn) choices
  -- | Link choices in "DiscussionChoices":
  insertChoice <- prepare conn "INSERT INTO \"DiscussionChoices\"(discussionid, informationid) VALUES (?, ?)"
  executeMany insertChoice $ map (\cId -> [did, toSql $ toId cId]) cIds

deleteInformation' :: (IConnection conn) => InformationId -> conn -> IO ()
deleteInformation' iid conn = do
  let q = "UPDATE \"Information\" SET deletion = CURRENT_TIMESTAMP WHERE informationid = ?"
  void $ quickQuery' conn q [toSql $ toId iid]

removeParticipant' :: (IConnection conn) => InformationId -> UserId -> conn -> IO ()
removeParticipant' iid uid conn = do
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

