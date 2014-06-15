{-# LANGUAGE RankNTypes #-}
module Carbon.Backend.PostgreSQL.Paging where

import qualified Data.List  as List
import qualified Data.Maybe as Maybe

import Carbon.Backend.PostgreSQL.Common
import Carbon.Backend.PostgreSQL.Conversion

itemCount :: Paging -> Query Count
itemCount p conn = do
  let (whr, vals) = mkWhere p
      q           = "SELECT COUNT(itemid) FROM items " ++ whr
  [[c]] <- quickQuery' conn q vals
  return $ fromSql c

pageItems :: Paging -> Query [ItemId]
pageItems p conn  = do
  let (whr, vals) = mkWhere p
      vals'       = vals++[toSql $ limit p, toSql $ offset p]
      q           = "SELECT itemid FROM items " ++ whr
                  ++"ORDER BY creation DESC LIMIT ? OFFSET ?"
  is <- quickQuery' conn q vals'
  return $ map (fromSql . head) is

mkWhere :: Paging -> (String, [SqlValue])
mkWhere p = let chk s v    = Maybe.isJust v ? (Just s, Nothing)
                article    = chk "(articleid IS NOT NULL) = ? "    $ isArticle    p
                deletion   = chk "(deletion IS NOT NULL) = ? "     $ isDeleted    p
                discussion = chk "(discussionid IS NOT NULL) = ? " $ isDiscussion p
                relation   = chk "(relationid IS NOT NULL) = ? "   $ isRelation   p
                result     = chk "(resultsetid IS NOT NULL) = ? "  $ isResult     p
                preds      = [article, deletion, discussion, relation, result]
                preds'     = List.intercalate " AND " $ Maybe.catMaybes preds
                preds''    = null preds' ? ("", "WHERE "++preds'++" ")
                vals       = map toSql $ Maybe.mapMaybe ($ p) [isArticle, isDeleted, isDiscussion, isRelation, isResult]
            in (preds'', vals)

userCount :: Query Count
userCount conn = do
  let q = "SELECT COUNT(userid) FROM users WHERE username != 'Nobody'"
  [[c]] <- quickQuery' conn q []
  return $ fromSql c

pageUsers :: Limit -> Offset -> Query [UserId]
pageUsers l o conn = do
  let q = "SELECT userid FROM users WHERE username != 'Nobody' ORDER BY creationtime DESC LIMIT ? OFFSET ?"
  us <- quickQuery' conn q [toSql l, toSql o]
  return $ map (fromSql . head) us
