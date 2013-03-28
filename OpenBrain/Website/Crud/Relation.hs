{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Crud.Relation (serve) where
{-|
  Accessing Relations the CRUD way
|-}
import Data.Maybe
import Happstack.Server as S
import System.Time (CalendarTime)

import OpenBrain.Website.Common
import OpenBrain.Website.Session (chkSession)
import qualified OpenBrain.Website.Parameters as Parameters

serve :: OBW Response
serve = msum [searchRelations, createRelation, readRelation, updateRelation, deleteRelation]

searchRelations :: OBW Response
searchRelations = crudRead . relationEndFromPath $ \rEnd ->
  informationIdFromPath $ \iid -> do
    rs <- liftOBB $ GetRelations iid rEnd Nothing False
    respOk $ jsonResponse rs

-- | New CRUD code below:
createRelation :: OBW Response
createRelation = crudCreate $ do
  rSource <- Parameters.getSource
  rTarget <- Parameters.getTarget
  t       <- Parameters.getType
  comment <- Parameters.getComment
  checkNotExists rSource rTarget t . attackOrDefense t .
    chkSession' $ \uid -> plusm addFail $ do
      rid <- liftOBB $ AddRelation rSource rTarget t comment
      fetchRelation rid $ respCreated . jsonResponse
  where
    addFail = respInternalServerError "Could not add Relation due to internal issues."

readRelation :: OBW Response
readRelation = crudRead . relationFromPath $ respOk . jsonResponse

updateRelation :: OBW Response
updateRelation = crudUpdate . relationFromPath $ \r -> do
  comment <- Parameters.getComment
  chkSession' $ \uid -> plusm upFail $ do
    liftOBB $ UpdateComment (relationId r) comment
    fetchRelation (relationId r) $ respOk . jsonResponse
  where
    upFail = respInternalServerError "Could not update Comment for Relation due to internal issues."

deleteRelation :: OBW Response
deleteRelation = crudDelete . relationFromPath $ \r ->
  attackOrDefense (relation r) . chkSession' $ \_ -> plusm delFail $ do
    liftOBB . DeleteRelation $ relationId r
    respOk "Relation deleted."
  where
    delFail = respInternalServerError "Could not delete Relation due to internal issues."

-- | CRUD helper:
checkNotExists :: Source -> Target -> RelationType -> OBW Response -> OBW Response
checkNotExists rSource rTarget t m = plusm alreadyExists $ do
  rs <- liftOBB $ GetRelations rSource RelationSource (Just t) False
  (guard . not $ any ((== rTarget) . target) rs) >> m
  where
    alreadyExists = respBadRequest "Relation already exists."

attackOrDefense :: RelationType -> OBW Response -> OBW Response
attackOrDefense t m = plusm wrongType $ guard (t `elem` [Attack, Defense]) >> m
  where wrongType = respBadRequest "RelationType must be Attack|Defense."

chkSession' :: (UserId -> OBW Response) -> OBW Response
chkSession' f = plusm invalidSession $ chkSession >>= f
  where
    invalidSession = respUnauthorized "Invalid session - you need to be logged in to modify Relations."

relationFromPath :: (Relation -> OBW Response) -> OBW Response
relationFromPath f = path $ \id -> do
  let rid = fromId id
  plusm (notFound rid) $ noMaybe (liftOBB $ GetRelation rid) >>= f
  where notFound rid = respNotFound . toResponse $ "Could not find Relation:\t" ++ show rid

fetchRelation :: RelationId -> (Relation -> OBW Response) -> OBW Response
fetchRelation rid f = plusm (fetchFail rid) $ do
  mR <- liftOBB $ GetRelation rid
  guard $ isJust mR
  f $ fromJust mR
  where
    fetchFail r = respInternalServerError  . toResponse $ "Could not fetch Relation from the Backend:\t" ++ show r

relationEndFromPath :: (RelationEnd -> OBW Response) -> OBW Response
relationEndFromPath f = path $ \rPath -> case rPath of
  "target" -> f RelationTarget
  "source" -> f RelationSource
  _        -> respBadRequest . toResponse $
    ("RelationEnd must be either target|source, not permitted:\t" ++ rPath :: String)

-- | Doesn't check for existence.
informationIdFromPath :: (InformationId -> OBW Response) -> OBW Response
informationIdFromPath f = path $ f . fromId
