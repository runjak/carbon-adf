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
  (source, target, rType) <- liftM3 (,,) (lookRead "source") (lookRead "target") $ lookRead "type"
  ndid <- Description.createDescription
  rid  <- liftB $ AddRelation ndid rType source target
  readRelation rid
  where
    createFail = respBadRequest $ responseJSON''
      "Login required; Expected parameters are: headline, description, source, target, type."

readRelation :: RelationId -> OBW Response
readRelation = respOk . responseJSON' <=< liftB . GetRelation
