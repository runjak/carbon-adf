{-# LANGUAGE RankNTypes #-}
module OpenBrain.Backend.PostgreSQL.Article where

import OpenBrain.Backend.PostgreSQL.Common
import OpenBrain.Backend.PostgreSQL.Description (addDescription, getDescription)
import OpenBrain.Backend.PostgreSQL.Relation as Relation
import OpenBrain.Data.Id

addArticle :: NewDescriptionId -> String -> Query ArticleId
addArticle ndid content conn = do
  let q = "INSERT INTO articles (descriptionid, content) VALUES (?, ?) RETURNING articleid"
  [[i]] <- quickQuery' conn q [toSql $ toId ndid, toSql content]
  return . fromId $ fromSql i

clone :: ArticleId -> Author -> Query ArticleId
clone aid uid conn = do
  -- | Copy Article and Description to new rows:
  a <- getArticle aid conn
  let d = aDescription a
  ndid <- addDescription uid (headline d) (description d) conn
  aid' <- addArticle ndid (content a) conn
  -- | Copy children from original:
  copyChildren <- prepare conn "INSERT INTO children (parent, child) VALUES (?, ?)"
  executeMany copyChildren . map (\c -> [toSql $ toId aid', toSql $ toId c]) $ children a
  -- | Add as child to original:
  quickQuery' conn "INSERT INTO children (parent, child) VALUES (?, ?)" [toSql $ toId aid, toSql $ toId aid']
  -- | Copy relations:
  let copyTarget = "INSERT INTO relations (descriptionid, source, target, type) "
                ++ "SELECT descriptionid, source, ?, type FROM relations WHERE target = ?"
      copySource = "INSERT INTO relations (descriptionid, source, target, type) "
                ++ "SELECT descriptionid, ?, target, type FROM relations WHERE source = ?"
      copies     = "SELECT relationid FROM relations WHERE target = ? OR source = ?"
  mapM_ (\q -> quickQuery' conn q [toSql $ toId aid', toSql $ toId aid]) [copyTarget, copySource]
  rids <- quickQuery' conn copies [toSql $ toId aid', toSql $ toId aid']
  forM_ rids $ (`forkD` conn) . fromId . fromSql . head
  -- | Done:
  return aid'
  where
    forkD :: RelationId -> Query ()
    forkD rid conn = do
      d  <- liftM rDescription $ getRelation rid conn
      d' <- addDescription (author d) (headline d) (description d) conn
      let q = "UPDATE relations SET descriptionid = ? WHERE relationid = ?"
      void $ quickQuery' conn q [toSql $ toId d', toSql $ toId rid]

getArticle :: ArticleId -> Query Article
getArticle aid conn = do
  let q = "SELECT descriptionid, content FROM articles WHERE articleid = ?"
  [[did, content]] <- quickQuery' conn q [toSql $ toId aid]
  description <- getDescription (fromId $ fromSql did) conn
  let getC = "SELECT child FROM children WHERE parent = ?"
  children <- liftM (map head) $ quickQuery' conn getC [toSql $ toId aid]
  return Article{
    articleId    = aid
  , content      = fromSql content
  , children     = map (fromId . fromSql) children
  , aDescription = description
  }

setContent :: ArticleId -> String -> Query ()
setContent aid content conn =
  let q = "UPDATE articles SET content = ? WHERE articleid = ?"
  in void $ quickQuery' conn q [toSql content, toSql $ toId aid]
