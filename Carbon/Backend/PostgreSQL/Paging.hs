{-# LANGUAGE RankNTypes #-}
module Carbon.Backend.PostgreSQL.Paging where

import Carbon.Backend.PostgreSQL.Common
import Carbon.Backend.PostgreSQL.Conversion

itemCount :: Paging -> Query Count
itemCount p conn = do
  let q = "SELECT COUNT(itemid) FROM items "
        ++"WHERE (articleid IS NOT NULL) = ? "
        ++"AND (deletion IS NOT NULL) = ? "
        ++"AND (discussionid IS NOT NULL) = ? "
        ++"AND (relationid IS NOT NULL) = ? "
        ++"AND (resultsetid IS NOT NULL) = ?"
  [[c]] <- quickQuery' conn q [ toSql $ isArticle p
                              , toSql $ isDeleted p
                              , toSql $ isDiscussion p
                              , toSql $ isRelation p
                              , toSql $ isResult p]
  return $ fromSql c

pageItems :: Paging -> Query [ItemId]
pageItems p conn = do
  let q = "SELECT itemid FROM items "
        ++"WHERE (articleid IS NOT NULL) = ? "
        ++"AND (deletion IS NOT NULL) = ? "
        ++"AND (discussionid IS NOT NULL) = ? "
        ++"AND (relationid IS NOT NULL) = ? "
        ++"AND (resultsetid IS NOT NULL) = ? "
        ++"ORDER BY creation DESC LIMIT ? OFFSET ?"
  is <- quickQuery' conn q [ toSql $ isArticle p
                           , toSql $ isDeleted p
                           , toSql $ isDiscussion p
                           , toSql $ isRelation p
                           , toSql $ isResult p
                           , toSql $ limit p
                           , toSql $ offset p]
  return $ map (fromSql . head) is

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
