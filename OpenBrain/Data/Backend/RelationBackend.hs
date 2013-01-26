{-# LANGUAGE GADTs #-}
module OpenBrain.Data.Backend.RelationBackend where

import OpenBrain.Data

data RBackendReq r where
  AddRelation    :: Source -> Target -> RelationType -> Comment -> RBackendReq RelationId
  DeleteRelation :: RelationId -> RBackendReq ()
  GetRelation    :: RelationId -> RBackendReq (Maybe Relation)
  -- | youngest first, deleted after non deleted
  GetRelations   :: InformationId -> RelationEnd -> Maybe RelationType -> AllowDeleted -> RBackendReq [Relation]
  UpdateComment  :: RelationId -> Comment -> RBackendReq ()
