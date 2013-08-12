{-# LANGUAGE RankNTypes #-}
module OpenBrain.Backend.PostgreSQL.Discussion where

import OpenBrain.Backend.PostgreSQL.Collection (getCollection)
import OpenBrain.Backend.PostgreSQL.Common
import OpenBrain.Backend.PostgreSQL.Relation (getRelation)
import OpenBrain.Backend.PostgreSQL.Result (getResults)
import OpenBrain.Data.Id

addDiscussion :: NewCollectionId -> [UserId] -> Maybe Timestamp -> Query DiscussionId
addDiscussion ncid uids deadline conn = do
  let q = "INSERT INTO discussions (collectionid, deadline) VALUES (?, ?) RETURNING discussionid"
  [[i]] <- quickQuery' conn q [toSql $ toId ncid, toSql deadline]
  addP <- prepare conn "INSERT INTO participants (discussionid, userid) VALUES (?, ?)"
  executeMany addP $ map (\u -> [i, toSql $ toId u]) uids
  return . fromId $ fromSql i

getDiscussion :: DiscussionId -> Query (Discussion ArticleId)
getDiscussion did conn = do
  let did' = [toSql $ toId did]
      q    = "SELECT collectionid, deadline FROM discussions WHERE discussionid = ?"
  [[cid, dline]] <- quickQuery' conn q did'
  collection <- getCollection (fromId $ fromSql cid) conn
  results <- getResults did conn
  parts <- quickQuery' conn "SELECT userid, voted FROM participants WHERE discussionid = ?" did'
  rs'   <- quickQuery' conn "SELECT relationid FROM relations WHERE discussionid = ?" did'
  rs    <- mapM (flip getRelation conn . fromId . fromSql . head) rs'
  return Discussion{
    discussionId = did
  , participants = map mkParts parts
  , deadline     = fromSql dline
  , relations    = rs
  , results      = results
  , dCollection  = collection
  }
  where
    mkParts :: [SqlValue] -> (UserId, Voted)
    mkParts [uid, v] = (fromId $ fromSql uid, fromSql v)

setParticipant :: DiscussionId -> UserId -> Bool -> Query ()
setParticipant did uid True conn =
  let q = "INSERT INTO participants (discussionid, userid) VALUES (?, ?)"
  in void $ quickQuery' conn q [toSql $ toId did, toSql $ toId uid]
setParticipant did uid False conn = 
  let q = "DELETE FROM participants WHERE discussionid = ? AND userid = ?"
  in void $ quickQuery' conn q [toSql $ toId did, toSql $ toId uid]
