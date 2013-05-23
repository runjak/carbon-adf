{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Discussion where

import OpenBrain.Website.Common
import qualified OpenBrain.Website.Description as Description
import qualified OpenBrain.Website.Session     as Session

pageDiscussions :: OBW Response
pageDiscussions = countAndPageBy DiscussionCount $ \l o -> liftM responseJSON' $ PageDiscussions l o

createDiscussion :: OBW Response
createDiscussion = Session.chkSession' $ \uid -> plusm createFail $ do
  (deadline, ndid) <- liftM2 (,) getDeadline Description.createDescription
  did <- liftB $ do
    ncid <- AddCollection ndid []
    AddDiscussion ncid [uid] deadline
  readDiscussion did
  where
    createFail = respBadRequest $ responseJSON "Expected parameters: headline, description, deadline"

readDiscussion :: DiscussionId -> OBW Response
readDiscussion = respOk . responseJSON' <=< liftB . GetDiscussion

joinDiscussion :: DiscussionId -> OBW Response
joinDiscussion did = Session.chkSession' $ \uid -> do
  liftB $ SetParticipant did uid True
  readDiscussion did

leaveDiscussion :: DiscussionId -> OBW Response
leaveDiscussion did = Session.chkSession' $ \uid -> do
  liftB $ SetParticipant did uid False
  readDiscussion did

weightRelation :: DiscussionId -> RelationId -> OBW Response
weightRelation did rid = Session.chkSession' $ \uid -> plusm weightFail $ do
  w <- getWeight
  liftB $ Weight did uid w rid
  readDiscussion did
  where
    weightFail = respBadRequest $ responseJSON "Expected parameter: weight."

-- | Parameters…
getDeadline :: OBW Timestamp
getDeadline = lookRead "deadline"

getWeight :: OBW Weight
getWeight = lookRead "weight"
