module OpenBrain.Website.Action.Discussion (serve) where
{-
  Actions to start and update discussions,
  and to become a participant in one and vote.
-}
import Happstack.Server as Server

import OpenBrain.Common
import OpenBrain.Data
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import OpenBrain.Website.Session
import qualified OpenBrain.Backend.Monad      as OBB
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
  iid <- liftOBB $ OBB.createDiscussion ci is dl dt
  liftOBB $ OBB.addParticipant iid uid
  handleSuccess $ "Created discussion: " ++ show iid

update :: OBW Response
update = Session.chkSession' $ \uid -> do
  iid <- Parameters.getInformationId
  is  <- Parameters.getItems
  ensureDiscussion iid $ do
    iid' <- liftOBB $ OBB.updateCollection iid is
    handleSuccess $ "Updated discussion: " ++ show iid'

setParticipant :: OBW Response
setParticipant = Session.chkSession' $ \uid -> do
  iid    <- Parameters.getInformationId
  status <- Parameters.getStatus
  ensureDiscussion iid $ do
    liftOBB $ OBB.setParticipant iid uid status
    handleSuccess "Participant status updated."

vote :: OBW Response
vote = Session.chkSession' $ \uid -> do
  iid <- Parameters.getInformationId
  liftOBB $ OBB.vote iid uid
  handleSuccess $ show uid ++ " voted on " ++ show iid

-- Helper functions:
ensureDiscussion :: InformationId -> OBW Response -> OBW Response
ensureDiscussion i f = do
  isD <- liftM (isDiscussion . media) . liftOBB $ OBB.getInformation i
  handleFail "Given Information is no discussion." $
    guard isD >> f

