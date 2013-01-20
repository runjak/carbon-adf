{-# LANGUAGE GADTs #-}
module OpenBrain.Backend.PostgreSQLBackend.RelationBackend where

import OpenBrain.Backend hiding (processRelation)
import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.PostgreSQLBackend.Sql.RelationBackend

processRelation :: PostgreSQLBackend -> RBackendReq r -> IO r
processRelation b (AddRelation s t rt c)                 = useBackend b $ addRelation' s t rt c
processRelation b (DeleteRelation rid)                   = useBackend b $ deleteRelation' rid
processRelation b (GetRelation rid)                      = useBackend b $ getRelation' rid
processRelation b (GetRelations iid rEnd rType aDeleted) = useBackend b $ getRelations' iid rEnd rType aDeleted
processRelation b (UpdateComment rid c)                  = useBackend b $ updateComment' rid c 
