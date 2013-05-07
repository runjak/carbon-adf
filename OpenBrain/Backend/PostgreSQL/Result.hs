module OpenBrain.Backend.PostgreSQL.Result where

import OpenBrain.Backend.PostgreSQL.Common
import OpenBrain.Data.Id

addResult :: DiscussionId -> [CollectionId] -> Query ResultId
addResult did cids conn = do
  -- | New entry in the results table:
  [[i]] <- quickQuery' conn "INSERT INTO results (resultid) VALUES (DEFAULT) RETURNING resultid" []
  -- | Set the results to belong to the discussion:
  quickQuery' conn "UPDATE discussions SET resultid = ? WHERE discussionid = ?" [i, toSql $ toId did]
  -- | Voters are the discussions participants:
  let voters = "INSERT INTO voters (resultid, userid) SELECT ?, userid FROM participants WHERE discussionid = ?"
  quickQuery' conn voters [i, toSql $ toId did]
  -- | Add choices to the result:
  addC <- prepare conn "INSERT INTO choices (resultid, collcetionid) VALUES (?, ?)"
  executeMany addC $ map (\c -> [i, toSql $ toId c]) cids
  -- | Done:
  return . fromId $ fromSql i

getResult :: ResultId -> Query Result
getResult rid conn = do
  choices <- quickQuery' conn "SELECT (collectionid, votes) FROM choices WHERE resultid = ?" [toSql $ toId rid]
  voters  <- quickQuery' conn "SELECT (userid, voted) FROM voters WHERE resultid = ?" [toSql $ toId rid]
  return Result{
    resultId = rid
  , choices  = concatMap mkChoice choices
  , voters   = concatMap mkVoter  voters
  }
  where
    mkChoice [cid, v] = [(fromId $ fromSql cid, fromSql v)]
    mkChoice _ = []
    mkVoter [uid, v] = [(fromId $ fromSql uid, fromSql v)]
    mkVoter _ = []

vote :: ResultId -> UserId -> CollectionId -> Query ()
vote rid uid cid conn = do
  let setVoter  = "UPDATE voters SET voted = true WHERE userid = ? AND resultid = ?"
      setChoice = "UPDATE choices SET votes = votes + 1 WHERE collectionid = ? AND resultId = ?"
  quickQuery' conn setVoter [toSql $ toId uid, toSql $ toId rid]
  quickQuery' conn setChoice [toSql $ toId cid, toSql $ toId rid]
  return ()
