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
addRelation' conn source target rtype comment = do
  stmt <- prepare conn "INSERT INTO \"Relations\" (comment, type, source, target) VALUES (?, ?, ?, ?)"
  execute stmt [toSql comment, toSql rtype, toSql $ toId source, toSql $ toId target]
  commit conn

deleteRelation' :: (IConnection conn) => conn -> RelationId -> IO ()
deleteRelation' conn relationid = do
  stmt <- prepare conn "UPDATE \"Relations\" SET deletion = CURRENT_TIMESTAMP WHERE relationid = ?"
  execute stmt [toSql $ toId relationid] >> commit conn

getRelations' :: (IConnection conn) => conn -> InformationId -> IO [Relation]
getRelations' conn iid = do
  let query = "SELECT relationid, comment, creation, deletion, type, source, target FROM \"Relations\" WHERE source = ?"
            ++" ORDER BY deletion DESC, creation DESC"
  rst <- quickQuery' conn query [toSql $ toId iid]
  return $ map go rst
  where
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
updateComment' conn rid comment = do
  stmt <- prepare conn "UPDATE \"Relations\" SET comment = ? WHERE relationid = ?"
  execute stmt [toSql comment, toSql $ toId rid] >> commit conn

