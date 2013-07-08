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
