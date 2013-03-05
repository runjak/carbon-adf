module OpenBrain.Website.Action.Relation (serve) where
{-
  Actions concerning Relations between Informations.
  All Actions here require users to be logged in.
-}
import Data.Maybe
import Happstack.Server as Server

import OpenBrain.Website.Common
import OpenBrain.Website.Session as Session

serve :: OBW Response
serve = msum [
    dir "addRelation"    addRelation
  , dir "deleteRelation" deleteRelation
  , dir "updateComment"  updateComment
  ]

{-
  Takes Target, Source, Type and Comment as parameters
  to create a new Relation between two Informations.
  Type may only be attack or defense.
  New Relation must not be one already existing.
-}
addRelation :: OBW Response
addRelation = do
  rSource <- getSource
  rTarget <- getTarget
  t       <- getType
  comment <- getComment
  handleFail "Relation already exists." $ do
    rs <- liftOBB $ GetRelations rSource RelationSource (Just t) False
    guard . not $ any ((== rTarget) . target) rs
    isAttackOrDefense t $
      Session.chkSession' $ \uid ->
        handleFail ("Relation type not allowed:\t" ++ show t) $ do
          guard $ t `elem` [Attack, Defense]
          handleFail "Problem in OpenBrain.Website.Action:addRelation" $ do
            rid <- liftOBB $ AddRelation rSource rTarget t comment
            jsonSuccess $ "Added Relation: " ++ show rid

{-
  Only allowed to delete Attack and Defense Relations.
-}
deleteRelation :: OBW Response
deleteRelation = do
  rid <- getRelationId
  r <- noMaybe . liftOBB $ GetRelation rid
  isAttackOrDefense (relation r) $
    Session.chkSession' $ \uid ->
      handleFail "Problem in OpenBrain.Website.Action.Relation:deleteRelation" $ do
        liftOBB $ DeleteRelation rid
        jsonSuccess $ "Deleted relation:\t" ++ show rid

{-
  Takes RelationId and Comment as parameters.
-}
updateComment :: OBW Response
updateComment = do
  comment <- getComment
  rid     <- getRelationId
  Session.chkSession' $ \uid ->
    handleFail "Problem in OpenBrain.Website.Action.Relation:updateComment" $ do
      liftOBB $ UpdateComment rid comment
      jsonSuccess $ "Updated comment on relation:\t" ++ show rid

{-
  Helper functions for above actions.
-}
isAttackOrDefense :: RelationType -> OBW Response -> OBW Response
isAttackOrDefense r response = handleFail "Relation must be of type Attack or Defense." $
  guard (r `elem` [Attack, Defense]) >> response

{-
  Fetching expected parameters
-}
getComment    = look "comment"                        :: OBW Comment
getRelationId = liftM fromId $ lookRead "relationId"  :: OBW RelationId
getSource     = liftM fromId $ lookRead "source"      :: OBW InformationId
getTarget     = liftM fromId $ lookRead "target"      :: OBW InformationId
getType       = lookRead "type"                       :: OBW RelationType

