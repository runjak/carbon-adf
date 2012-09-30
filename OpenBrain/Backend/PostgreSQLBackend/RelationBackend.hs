module OpenBrain.Backend.PostgreSQLBackend.RelationBackend () where

import Control.Monad
import Database.HDBC as H

import OpenBrain.Backend
import OpenBrain.Backend.PostgreSQLBackend.Convertibles ()
import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.Types as Types
import OpenBrain.Data.Id
import OpenBrain.Data.Relation

instance RelationBackend PostgreSQLBackend where
  addRelation     b = withWConn (conn b) addRelation'
  deleteRelation  b = withWConn (conn b) deleteRelation'
  getRelations    b = withWConn (conn b) getRelations'
  updateComment   b = withWConn (conn b) updateComment'

addRelation' :: (IConnection conn) => conn -> Types.Source -> Types.Target -> RelationType -> Types.Comment -> IO ()  
addRelation' conn source target rtype comment = withTransaction conn $ \conn -> do
  stmt <- prepare conn "INSERT INTO \"Relations\" (comment, type, source, target) VALUES (?, ?, ?, ?)"
  execute stmt [toSql comment, toSql rtype, toSql $ toId source, toSql $ toId target]
  return ()

deleteRelation' :: (IConnection conn) => conn -> RelationId -> IO ()
deleteRelation' conn relationid = withTransaction conn $ \conn ->  do
  stmt <- prepare conn "UPDATE \"Relations\" SET deletion = CURRENT_TIMESTAMP WHERE relationid = ?"
  execute stmt [toSql $ toId relationid] >> return ()

getRelations' :: (IConnection conn) => conn -> InformationId -> Types.RelationEnd -> Maybe RelationType -> Types.AllowDeleted -> IO [Relation]
getRelations' conn iid rEnd mRType aDeleted = do
    rst <- query conn (toSql $ toId iid) rEnd (liftM toSql mRType) aDeleted
    return $ map go rst
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

    go :: [SqlValue] -> Relation
    go [rid, comment, creation, deletion, relationid, source, target] = Relation {
        comment     = fromSql comment
      , creation    = fromSql creation
      , deletion    = fromSql deletion
      , relation    = fromSql relationid
      , relationId  = fromId $ fromSql rid
      , source      = fromId $ fromSql source
      , target      = fromId $ fromSql target
      }
    go _ = error "Malformed result in OpenBrain.Backend.MysqlBackend.RelationBackend.getRelations'."

updateComment' :: (IConnection conn) => conn -> RelationId -> Types.Comment -> IO ()
updateComment' conn rid comment = withTransaction conn $ \conn -> do
  stmt <- prepare conn "UPDATE \"Relations\" SET comment = ? WHERE relationid = ?"
  execute stmt [toSql comment, toSql $ toId rid] >> return ()

