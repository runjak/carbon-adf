{-# LANGUAGE NoMonomorphismRestriction, RankNTypes #-}
module OpenBrain.Backend.PostgreSQL.Paging(
  articleCount, collectionCount, descriptionCount, discussionCount, relationCount, resultCount, userCount
, pageArticles, pageCollections, pageDescriptions, pageDiscussions, pageRelations, pageResults, pageUsers
)where

import OpenBrain.Backend.PostgreSQL.Common
import OpenBrain.Data.Id

articleCount = getCount $ "SELECT COUNT(*) "
  ++ "FROM articles a INNER JOIN descriptions d USING (descriptionid) "
  ++ "WHERE d.deletion IS NULL "
  ++ "AND (a.content != '' OR d.description != '')"
collectionCount  = getCount $ "SELECT COUNT(*) "
  ++ "FROM collections INNER JOIN descriptions d USING (descriptionid) "
  ++ "WHERE d.deletion IS NULL"
descriptionCount = getCount "SELECT COUNT(*) FROM descriptions WHERE deletion IS NULL"
discussionCount  = getCount $ "SELECT COUNT(*) "
  ++ "FROM discussions INNER JOIN collections USING (collectionid) "
  ++ "INNER JOIN descriptions d USING (descriptionid) WHERE d.deletion IS NULL"
relationCount    = getCount $ "SELECT COUNT(*) "
  ++ "FROM relations INNER JOIN descriptions d USING (descriptionid) "
  ++ "WHERE d.deletion IS NULL"
resultCount      = getCount "SELECT COUNT(*) FROM results"
userCount        = getCount "SELECT COUNT(*) FROM users WHERE username != 'Nobody'"

getCount :: String -> Query Count
getCount qString conn = do
  rst <- quickQuery' conn qString []
  case rst of
    [[c]] -> return $ fromSql c
    _ -> return 0

pageArticles'     = "SELECT a.articleid "
  ++ "FROM articles a JOIN descriptions d USING (descriptionid) "
  ++ "WHERE d.deletion IS NULL "
  ++ "AND (a.content != '' OR d.description != '') "
  ++ "ORDER BY d.creation DESC LIMIT ? OFFSET ?"
pageCollections'  = "SELECT c.collectionid "
  ++ "FROM collections c JOIN descriptions d USING (descriptionid) "
  ++ "WHERE d.deletion IS NULL LIMIT ? OFFSET ?"
pageDescriptions' = "SELECT descriptionid FROM descriptions WHERE deletion IS NULL LIMIT ? OFFSET ?"
pageDiscussions'  = "SELECT dis.discussionid "
  ++ "FROM discussions dis JOIN collections USING (collectionid) JOIN descriptions d USING (descriptionid) "
  ++ "WHERE d.deletion IS NULL LIMIT ? OFFSET ?"
pageRelations'    = "SELECT r.relationid "
  ++ "FROM relations r JOIN descriptions d USING (descriptionid) "
  ++ "WHERE d.deletion IS NULL LIMIT ? OFFSET ?"
pageResults'      = "SELECT resultid FROM results LIMIT ? OFFSET ?"
pageUsers'        = "SELECT userid FROM users WHERE username != 'Nobody' LIMIT ? OFFSET ?"

pageArticles     = idQ pageArticles'     :: Limit -> Offset -> Query [ArticleId]
pageCollections  = idQ pageCollections'  :: Limit -> Offset -> Query [CollectionId]
pageDescriptions = idQ pageDescriptions' :: Limit -> Offset -> Query [DescriptionId]
pageDiscussions  = idQ pageDiscussions'  :: Limit -> Offset -> Query [DiscussionId]
pageRelations    = idQ pageRelations'    :: Limit -> Offset -> Query [RelationId]
pageResults      = idQ pageResults'      :: Limit -> Offset -> Query [ResultId]
pageUsers        = idQ pageUsers'        :: Limit -> Offset -> Query [UserId]

idQ :: IdType i => String -> Limit -> Offset -> Query [i]
idQ q l o conn = do
  rst <- quickQuery' conn q [toSql l, toSql o] 
  return $ map (\[i] -> fromId $ fromSql i) rst
