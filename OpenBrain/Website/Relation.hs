{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Relation(
  pageRelations
, createRelation
, readRelation
)where

import OpenBrain.Website.Common
import qualified OpenBrain.Website.Description as Description
import qualified OpenBrain.Website.Session     as Session

pageRelations :: OBW Response
pageRelations = countAndPageBy RelationCount $ \l o -> liftM responseJSON' $ PageRelations l o

createRelation :: OBW Response
createRelation = plusm createFail $ do
  (discussion, source, target) <- liftM3 (,,) (lookRead "discussion") (lookRead "source") (lookRead "target")
  ndid <- Description.createDescription
  rid  <- liftB $ AddRelation discussion ndid source target
  readRelation rid
  where
    createFail = respBadRequest $ responseJSON''
      "Login required; Expected parameters are: headline, description, discussion, source, target."

readRelation :: RelationId -> OBW Response
readRelation = respOk . responseJSON' <=< liftB . GetRelation
