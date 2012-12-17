module OpenBrain.Backend.PostgreSQLBackend.Sql.RelationBackend where

import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.Types as Types
import OpenBrain.Data.Id
import OpenBrain.Data.Relation

addRelation' :: (IConnection conn) => Types.Source -> Types.Target -> RelationType -> Types.Comment -> conn -> IO RelationId
addRelation' source target rtype comment conn = do
  stmt <- prepare conn "INSERT INTO \"Relations\" (comment, type, source, target) VALUES (?, ?, ?, ?)"
  execute stmt [toSql comment, toSql rtype, toSql $ toId source, toSql $ toId target]
  [[rid]] <- quickQuery' conn "SELECT LASTVAL()" []
  return . fromId $ fromSql rid

deleteRelation' :: (IConnection conn) => RelationId -> conn -> IO ()
deleteRelation' relationid conn = do
  stmt <- prepare conn "UPDATE \"Relations\" SET deletion = CURRENT_TIMESTAMP WHERE relationid = ?"
  void $ execute stmt [toSql $ toId relationid]

getRelation' :: (IConnection conn) => RelationId -> conn -> IO (Maybe Relation)
getRelation' rid conn = do
  let q = "SELECT relationid, comment, creation, deletion, type, source, target FROM \"Relations\" WHERE relationid = ?"
  rst <- liftIO $ quickQuery' conn q [toSql $ toId rid]
  guard . not $ null rst
  return . Just . mkRelation $ head rst

getRelations' :: (IConnection conn) => InformationId -> Types.RelationEnd
                                    -> Maybe RelationType -> Types.AllowDeleted
                                    -> conn -> IO [Relation]
getRelations' iid rEnd mRType aDeleted conn = do
    rst <- query conn (toSql $ toId iid) rEnd (liftM toSql mRType) aDeleted
    return $ map mkRelation rst
  where
    query :: (IConnection conn) => conn -> SqlValue -> RelationEnd -> Maybe SqlValue -> Types.AllowDeleted -> IO [[SqlValue]]
    query conn iid RelationSource (Just t) True   = do
      let q = "SELECT relationid, comment, creation, deletion, type, source, target FROM \"Relations\" WHERE "
           ++ "source = ? AND type = ? ORDER BY deletion DESC, creation DESC"
      quickQuery' conn q [iid, t]
    query conn iid RelationSource (Just t) False  = do
      let q = "SELECT relationid, comment, creation, deletion, type, source, target FROM \"Relations\" WHERE "
           ++ "source = ? AND type = ? AND deletion IS NULL ORDER BY creation DESC"
      quickQuery' conn q [iid, t]
    query conn iid RelationSource Nothing  True   = do
      let q = "SELECT relationid, comment, creation, deletion, type, source, target FROM \"Relations\" WHERE "
           ++ "source = ? ORDER BY deletion DESC, creation DESC"
      quickQuery' conn q [iid]
    query conn iid RelationSource Nothing  False  = do
      let q = "SELECT relationid, comment, creation, deletion, type, source, target FROM \"Relations\" WHERE "
           ++ "source = ? AND deletion IS NULL ORDER BY creation DESC"
      quickQuery' conn q [iid]
    query conn iid RelationTarget (Just t) True   = do
      let q = "SELECT relationid, comment, creation, deletion, type, source, target FROM \"Relations\" WHERE "
           ++ "target = ? AND type = ? ORDER BY deletion DESC, creation DESC"
      quickQuery' conn q [iid, t]
    query conn iid RelationTarget (Just t) False  = do
      let q = "SELECT relationid, comment, creation, deletion, type, source, target FROM \"Relations\" WHERE "
           ++ "target = ? AND type = ? AND deletion IS NULL ORDER BY creation DESC"
      quickQuery' conn q [iid, t]
    query conn iid RelationTarget Nothing  True   = do
      let q = "SELECT relationid, comment, creation, deletion, type, source, target FROM \"Relations\" WHERE "
           ++ "target = ? ODER BY deletion DESC, creation DESC"
      quickQuery' conn q [iid]
    query conn iid RelationTarget Nothing  False  = do
      let q = "SELECT relationid, comment, creation, deletion, type, source, target FROM \"Relations\" WHERE "
           ++ "target = ? AND deletion IS NULL ORDER BY creation DESC"
      quickQuery' conn q [iid]

updateComment' :: (IConnection conn) => RelationId -> Types.Comment -> conn -> IO ()
updateComment' rid comment conn = do
  stmt <- prepare conn "UPDATE \"Relations\" SET comment = ? WHERE relationid = ?"
  void $ execute stmt [toSql comment, toSql $ toId rid]

mkRelation :: [SqlValue] -> Relation
mkRelation [rid, comment, creation, deletion, relationid, source, target] = Relation {
    comment     = fromSql comment
  , creation    = fromSql creation
  , deletion    = fromSql deletion
  , relation    = fromSql relationid
  , relationId  = fromId $ fromSql rid
  , source      = fromId $ fromSql source
  , target      = fromId $ fromSql target
  }
mkRelation _ = error "Malformed result in OpenBrain.Backend.MysqlBackend.RelationBackend:mkRelation"
