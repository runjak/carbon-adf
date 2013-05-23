{-# LANGUAGE RankNTypes #-}
module OpenBrain.Backend.PostgreSQL.Collection where

import OpenBrain.Backend.PostgreSQL.Common
import OpenBrain.Backend.PostgreSQL.Description (getDescription)
import OpenBrain.Data.Id

addCollection :: NewDescriptionId -> [ArticleId] -> Query NewCollectionId
addCollection ndid as conn = do
  let q = "INSERT INTO collections (descriptionid) VALUES (?) RETURNING collectionid"
  [[i]] <- quickQuery' conn q [toSql $ toId ndid]
  let cid = fromId $ fromSql i
  collectArticles cid as conn
  return . fromId $ toId cid

collectArticles :: CollectionId -> [ArticleId] -> Query ()
collectArticles cid as conn = do
  add <- prepare conn "INSERT INTO collectedarticles (collectionid, articleid) VALUES (?, ?)"
  executeMany add $ map (\a -> [toSql $ toId cid, toSql $ toId a]) as

forgetArticles :: CollectionId -> [ArticleId] -> Query()
forgetArticles cid as conn = do
  del <- prepare conn "DELETE FROM collectedarticles WHERE collectionid = ? AND articleid = ?"
  executeMany del $ map (\a -> [toSql $ toId cid, toSql $ toId a]) as

getCollection :: CollectionId -> Query Collection
getCollection cid conn = do
  articles <- quickQuery' conn "SELECT articleid FROM collectedarticles WHERE collectionid = ?" [toSql $ toId cid]
  [[did]]  <- quickQuery' conn "SELECT descriptionid FROM collections WHERE collectionid = ?" [toSql $ toId cid]
  description <- getDescription (fromId $ fromSql did) conn
  return Collection{
    collectionId = cid
  , articles     = map (fromId . fromSql . head) articles
  , cDescription = description
  }
