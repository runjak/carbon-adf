module OpenBrain.Website.Action.Discussion (serve) where
{-
  Actions to start and update discussions,
  and to become a participant in one and vote.
-}
import Data.Maybe
import Happstack.Server as Server

import OpenBrain.Website.Common
import OpenBrain.Website.Session
import qualified OpenBrain.Website.Parameters as Parameters
import qualified OpenBrain.Website.Session    as Session

serve :: OBW Response
serve = msum [
    dir "create"          create
  , dir "update"          update
  , dir "setParticipant"  setParticipant
  , dir "vote"            vote
  ]

create :: OBW Response
create = Session.chkSession' $ \uid -> do
  ci  <- liftM2 (CreateInformation uid) Parameters.getTitle Parameters.getDescription
  is  <- Parameters.getItems
  dl  <- Parameters.getDeadline
  dt  <- Parameters.getDiscussionType
  iid <- liftOBB $ CreateDiscussion ci is dl dt
  liftOBB $ AddParticipant iid uid
  jsonSuccess $ "Created discussion: " ++ show iid

update :: OBW Response
update = Session.chkSession' $ \uid -> do
  iid <- Parameters.getInformationId
  is  <- Parameters.getItems
  ensureDiscussion iid $ do
    iid' <- liftOBB $ UpdateCollection iid is
    jsonSuccess $ "Updated discussion: " ++ show iid'

setParticipant :: OBW Response
setParticipant = Session.chkSession' $ \uid -> do
  iid    <- Parameters.getInformationId
  status <- Parameters.getStatus
  ensureDiscussion iid $ do
    liftOBB $ SetParticipant iid uid status
    jsonSuccess "Participant status updated."

vote :: OBW Response
vote = Session.chkSession' $ \uid -> do
  iid <- Parameters.getInformationId
  liftOBB $ Vote iid uid
  jsonSuccess $ show uid ++ " voted on " ++ show iid

-- Helper functions:
ensureDiscussion :: InformationId -> OBW Response -> OBW Response
ensureDiscussion i f = do
  isD <- liftM (isDiscussion . media . fromJust) . liftOBB $ GetInformation i
  handleFail "Given Information is no discussion." $
    guard isD >> f

