{-# LANGUAGE RankNTypes #-}
module OpenBrain.Backend.PostgreSQL.Relation where

import OpenBrain.Backend.PostgreSQL.Common
import OpenBrain.Backend.PostgreSQL.Description (getDescription)
import OpenBrain.Data.Id

addRelation :: DiscussionId -> NewDescriptionId -> ArticleId -> ArticleId -> Query RelationId
addRelation did ndid source target conn = do
  let q = "INSERT INTO relations (discussionid, descriptionid, source, target) VALUES (?, ?, ?, ?) RETURNING relationid"
  [[i]] <- quickQuery' conn q . map toSql $ [toId did, toId ndid, toId source, toId target]
  return . fromId $ fromSql i

getRelation :: RelationId -> Query Relation
getRelation rid conn = do
  let q = "SELECT descriptionid, source, target FROM relations WHERE relationid = ?"
  [[did, source, target]] <- quickQuery' conn q [toSql $ toId rid]
  description <- getDescription (fromId $ fromSql did) conn
  return Relation{
    relationId   = rid
  , source       = fromId $ fromSql source
  , target       = fromId $ fromSql target
  , rDescription = description
  }

relationDiscussion :: RelationId -> Query DiscussionId
relationDiscussion rid conn = do
  let q = "SELECT discussionid FROM relations WHERE relationid = ?"
  [[did]] <- quickQuery' conn q [toSql $ toId rid]
  return . fromId $ fromSql did

-- | FIXME check what should happen to the left behind description.
removeRelation :: RelationId -> Query ()
removeRelation rid conn =
  let q = "DELETE FROM relations WHERE relationid = ?"
  in void $ quickQuery' conn q [toSql $ toId rid]
