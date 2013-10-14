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
  putStrLn "OpenBrain.Backend.PostgreSQL.Article:clone"
  -- | Copy Article and Description to new rows:
  a <- getArticle aid conn
  let d = aDescription a
  ndid <- addDescription uid (headline d) (description d) conn
  aid' <- addArticle ndid (content a) conn
  -- | Copy parents from original:
  let copyParents = "INSERT INTO children (parent, child) SELECT parent, ? FROM children WHERE child = ?"
  quickQuery' conn copyParents [toSql $ toId aid', toSql $ toId aid]
  -- | Add as child to original:
  quickQuery' conn "INSERT INTO children (parent, child) VALUES (?, ?)" [toSql $ toId aid, toSql $ toId aid']
  -- | Copy relations:
  let copyTarget = "INSERT INTO relations (descriptionid, discussionid, source, target) "
                ++ "SELECT descriptionid, discussionid, source, ? FROM relations WHERE target = ?"
      copySource = "INSERT INTO relations (descriptionid, discussionid, source, target) "
                ++ "SELECT descriptionid, discussionid, ?, target FROM relations WHERE source = ?"
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
  rst <- quickQuery' conn q [toSql $ toId aid]
  when (null rst) . error $ "Non existent ArticleId: " ++ show aid
  let [[did, content]] = rst
  description <- getDescription (fromId $ fromSql did) conn
  let getC = "SELECT child FROM children WHERE parent = ?"
  children <- liftM (map head) $ quickQuery' conn getC [toSql $ toId aid]
  let getP = "SELECT parent FROM children WHERE child = ?"
  parents <- liftM (map head) $ quickQuery' conn getP [toSql $ toId aid]
  return Article{
    articleId    = aid
  , content      = fromSql content
  , children     = map (fromId . fromSql) children
  , parents      = map (fromId . fromSql) parents
  , aDescription = description
  }

setContent :: ArticleId -> String -> Query ()
setContent aid content conn =
  unless (null content) $
    let q = "UPDATE articles SET content = ? WHERE articleid = ?"
    in void $ quickQuery' conn q [toSql content, toSql $ toId aid]

replaceDummy :: ArticleId -> ArticleId -> Query Bool
replaceDummy dummy aid conn = do
  dummyA <- getArticle dummy conn
  if isDummy dummyA then process (descriptionId $ aDescription dummyA) else return False
  where
    {- We don't care if the dummy is a profile or part of a parent-child chain,
       because that should never be the case.-}
    process did = do
      let qs = [ "UPDATE collectedarticles SET articleid = ? WHERE articleid = ?"
               , "UPDATE relations SET target = ? WHERE target = ?"
               , "UPDATE relations SET source = ? WHERE target = ?"]
      mapM_ (\q -> quickQuery' conn q [toSql $ toId aid, toSql $ toId dummy]) qs
      quickQuery' conn "DELETE FROM relations WHERE source = ? AND target = ?" [toSql $ toId aid, toSql $ toId aid]
      quickQuery' conn "DELETE FROM descriptions WHERE descriptionid = ?" [toSql $ toId did]
      quickQuery' conn "DELETE FROM articles WHERE articleid = ?" [toSql $ toId dummy]
      return True
