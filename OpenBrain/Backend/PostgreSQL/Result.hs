{-# LANGUAGE RankNTypes #-}
module OpenBrain.Backend.PostgreSQL.Result where

import OpenBrain.Backend.PostgreSQL.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Logic (ResultType)

addResult :: DiscussionId -> ResultType -> [(ResultState, ArticleId)] -> Query ResultId
addResult did rType rArticles conn = do
  let did' = toSql $ toId did
  -- | New entry in the results table:
  let q = "INSERT INTO results (discussionid, resulttype) VALUES (?, ?) RETURNING resultid"
  [[i]] <- quickQuery' conn q [did', toSql $ fromEnum rType]
  -- | Inserting rArticles:
  let q  = "INSERT INTO resultarticles (resultid, state, articleid) VALUES (?, ?, ?)"
      qs = map (\(rState, aid) -> [i, toSql $ fromEnum rState, toSql $ toId aid])
  addRArticle <- prepare conn q
  executeMany addRArticle $ qs rArticles 
  -- | Done:
  return . fromId $ fromSql i

getResults :: DiscussionId -> Query [Result]
getResults did conn = do
  let q = "SELECT resultid, resulttype, votes FROM results WHERE discussionid = ?"
  mapM (go conn) =<< quickQuery' conn q [toSql $ toId did]
  where
    go :: IConnection conn => conn -> [SqlValue] -> IO Result
    go conn [rid, rtype, votes] = do
      let q = "SELECT state, articleid FROM resultarticles WHERE resultid = ?"
      rArticles <- quickQuery' conn q [rid]
      return Result{
        resultId   = fromId $ fromSql rid
      , resultType = toEnum $ fromSql rtype
      , rArticles  = map mkRArticles rArticles
      , votes      = fromSql votes
      }

    mkRArticles :: [SqlValue] -> (ResultState, ArticleId)
    mkRArticles [state, aid] = (toEnum $ fromSql state, fromId $ fromSql aid)

vote :: ResultId -> UserId -> Query ()
vote rid uid conn = do
  let setVoted = "UPDATE participants SET voted = true "
              ++ "WHERE discussionid = (SELECT discussionid "
              ++ "FROM results WHERE resultid = ?) AND userid = ?"
      incVotes = "UPDATE results SET votes = votes + 1 WHERE resultid = ?"
      rid'     = toSql $ toId rid
  quickQuery' conn setVoted [rid', toSql $ toId uid]
  quickQuery' conn incVotes [rid']
  return ()

removeResults :: DiscussionId -> Query ()
removeResults did conn = do
  let resetVotes  = "UPDATE participants SET voted = false WHERE discussionid = ?"
      removeRArts = "DELETE FROM resultarticles WHERE "
                 ++ "resultid = ANY (SELECT resultid FROM results WHERE discussionid = ?)"
      remResults  = "DELETE FROM results WHERE discussionid = ?"
  forM_ [resetVotes, removeRArts, remResults] $ \q -> quickQuery' conn q [toSql $ toId did]

disForResult :: ResultId -> Query DiscussionId
disForResult rid conn = do
  let q = "SELECT discussionid FROM results WHERE resultid = ?"
  did <- quickQuery' conn q [toSql $ toId rid]
  when (null did) . fail $ "Non existent resultId: " ++ show rid
  return . fromId . fromSql . head $ head did
