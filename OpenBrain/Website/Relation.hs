{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Relation where

import OpenBrain.Website.Common
import qualified OpenBrain.Backend.Logic       as BLogic
import qualified OpenBrain.Website.Description as Description
import qualified OpenBrain.Website.Session     as Session

pageRelations :: OBW Response
pageRelations = countAndPageBy RelationCount $ \l o -> liftM responseJSON' $ PageRelations l o

createRelation :: OBW Response
createRelation = plusm createFail $ do
  (discussion, source, target) <- liftM3 (,,) (lookRead "discussion") (lookRead "source") (lookRead "target")
  ndid <- Description.createDescription
  rid  <- liftB $ AddRelation discussion ndid source target
  liftB $ BLogic.autoCondition discussion target
  readRelation rid
  where
    createFail = respBadRequest $ responseJSON''
      "Login required; Expected parameters are: headline, description, discussion, source, target."

readRelation :: RelationId -> OBW Response
readRelation = respOk . responseJSON' <=< liftB . GetRelation

updateRelation :: RelationId -> OBW Response
updateRelation rid = Session.chkSession' . const $ do
  r <- liftB $ GetRelation rid
  let did = descriptionId $ rDescription r
  success <- liftM or $ mapM ($ did) [updateHeadline, updateDescription]
  success ? (readRelation rid, updateFail)
  where
    updateHeadline :: DescriptionId -> OBW Bool
    updateHeadline did = plusm (return False) $ do
      h <- look "headline"
      liftB $ SetHeadline did h
      return True

    updateDescription :: DescriptionId -> OBW Bool
    updateDescription did = plusm (return False) $ do
      d <- look "description"
      liftB $ SetDescription did d
      return True

    updateFail = respBadRequest $ responseJSON'' "Expected parameters are: headline, description"

deleteRelation :: RelationId -> OBW Response
deleteRelation rid = Session.chkSession' . const $ do
  liftB  $ BLogic.removeRelation rid
  respOk $ responseJSON'' "Relation removed"
