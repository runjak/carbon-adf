{-# LANGUAGE RankNTypes #-}
module OpenBrain.Backend.PostgreSQL.Relation where

import OpenBrain.Backend.PostgreSQL.Common
import OpenBrain.Backend.PostgreSQL.Description (getDescription)
import OpenBrain.Data.Id

addRelation :: NewDescriptionId -> RelationType -> ArticleId -> ArticleId -> Query RelationId
addRelation ndid rtype source target conn = do
  let q = "INSERT INTO relations (descriptionid, source, target, type) VALUES (?, ?, ?, ?) RETURNING relationid"
  [[i]] <- quickQuery' conn q [toSql $ toId ndid, toSql $ toId source, toSql $ toId target, toSql rtype]
  return . fromId $ fromSql i

getRelation :: RelationId -> Query Relation
getRelation rid conn = do
  let q = "SELECT descriptionid, source, target, type FROM relations WHERE relationid = ?"
  [[did, source, target, rtype]] <- quickQuery' conn q [toSql $ toId rid]
  description <- getDescription (fromId $ fromSql did) conn
  return Relation{
    relationId   = rid
  , source       = fromId $ fromSql source
  , target       = fromId $ fromSql target
  , rType        = fromSql rtype
  , rDescription = description
  }
