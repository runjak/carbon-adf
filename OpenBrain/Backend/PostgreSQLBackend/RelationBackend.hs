module OpenBrain.Backend.PostgreSQLBackend.RelationBackend () where

import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.PostgreSQLBackend.Sql.RelationBackend
import OpenBrain.Backend.Types as Types
import OpenBrain.Data.Id
import OpenBrain.Data.Relation

instance RelationBackend PostgreSQLBackend where
  addRelation source target rtype comment b = useBackend b $ addRelation' source target rtype comment
  deleteRelation rid b                      = useBackend b $ deleteRelation' rid
  getRelation rid b                         = useBackend b $ getRelation' rid
  getRelations iid rEnd rType aDeleted b    = useBackend b $ getRelations' iid rEnd rType aDeleted
  updateComment rid comment b               = useBackend b $ updateComment' rid comment

