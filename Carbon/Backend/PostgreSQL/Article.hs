{-# LANGUAGE RankNTypes #-}
module Carbon.Backend.PostgreSQL.Article where

import Data.Monoid (Monoid(..))
import qualified Data.Maybe as Maybe

import Carbon.Backend.PostgreSQL.Common
import Carbon.Backend.PostgreSQL.Conversion

getArticle :: SqlValue -> Query (Maybe Article)
getArticle SqlNull _ = return Nothing
getArticle aid conn = do
  let getA = "SELECT content FROM articles WHERE articleid = ?"
  [[c]] <- quickQuery' conn getA [aid]
  return . Just $ Article {
    articleId = fromSql aid
  , content   = fromSql c
  }

setArticle :: IConnection conn => conn -> Item Id -> IO (Either Error (Item Id))
setArticle conn i 
  | Maybe.isNothing (article i) = do -- Setting article to null.
    let q = "UPDATE items SET articleid = NULL WHERE itemid = ?"
    quickQuery' conn q [toSql $ itemId i]
    return $ Right i
  | noId . articleId $ getA i = do -- Creating an article.
    let a = getA i
        q = "INSERT INTO articles (content) VALUES (?) RETURNING articleid"
    [[aid]] <- quickQuery' conn q [toSql $ content a]
    let q = "UPDATE items SET articleid = ? WHERE itemid = ?"
        aid' = fromSql aid :: Id
    quickQuery' conn q [aid, toSql $ itemId i]
    return . Right $ i <+ a <+ aid'
  | otherwise = do -- Updating the article.
    let a = getA i
        q = "SELECT COUNT(*) > 0 FROM articles WHERE articleid = ? AND content = ?"
    [[c]] <- quickQuery' conn q [toSql $ articleId a, toSql $ content a]
    let boring = return $ Right i
        changed = setArticle conn $ i <+ a <+ (mempty :: Id)
    fromSql c ? (boring, changed)
  where
    getA = Maybe.fromJust . article
