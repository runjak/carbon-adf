{-# LANGUAGE RankNTypes #-}
module OpenBrain.Backend.PostgreSQL.Collection where

import OpenBrain.Backend.PostgreSQL.Common
import OpenBrain.Data.Id
import qualified OpenBrain.Data.Logic                     as Logic
import qualified OpenBrain.Backend.PostgreSQL.Article     as Article
import qualified OpenBrain.Backend.PostgreSQL.Description as Description

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
  [[did]] <- quickQuery' conn "SELECT descriptionid FROM collections WHERE collectionid = ?" [toSql $ toId cid]
  desc    <- Description.getDescription (fromId $ fromSql did) conn
  let q = "SELECT articleid, pos_x, pos_y, accepted, condition, customcondition "
       ++ "FROM collectedarticles WHERE collectionid = ?"
  cArts   <- mapM (go conn) =<< quickQuery' conn q [toSql $ toId cid]
  return Collection{
    collectionId = cid
  , articles     = cArts
  , cDescription = desc
  }
  where
    go conn [aid, x, y, acc, cond, cust] = do
      a <- Article.getArticle (fromId $ fromSql aid) conn
      return CollectionArticle{
        cArticle        = a
      , posX            = fromSql x
      , posY            = fromSql y
      , accepted        = fromSql acc
      , customcondition = fromSql cust
      , condition       = Nothing -- read $ fromSql cond
      }
