module OpenBrain.Backend.MysqlBackend.InformationBackend () where

import Control.Monad
import Database.HDBC as H

import OpenBrain.Backend
import OpenBrain.Backend.MysqlBackend.Convertibles ()
import OpenBrain.Backend.MysqlBackend.Common
import OpenBrain.Backend.Types as Types
import OpenBrain.Data.Id
import OpenBrain.Data.Relation

instance RelationBackend MysqlBackend where
  addRelation     b = withWConn (conn b) addRelation'
  deleteRelation  b = withWConn (conn b) deleteRelation'
  getRelations    b = withWConn (conn b) getRelations'
  updateComment   b = withWConn (conn b) updateComment'


addRelation' :: (IConnection conn) => conn -> Types.Source -> Types.Target -> RelationType -> Types.Comment -> IO ()  
addRelation' = undefined

deleteRelation' :: (IConnection conn) => conn -> RelationId -> IO ()
deleteRelation' = undefined

getRelations' :: (IConnection conn) => conn -> InformationId -> IO [Relation]
getRelations' = undefined

updateComment' :: (IConnection conn) => conn -> RelationId -> Types.Comment -> IO ()
updateComment' = undefined
