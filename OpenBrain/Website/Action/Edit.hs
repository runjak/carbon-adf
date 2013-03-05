module OpenBrain.Website.Action.Edit (serve) where
{-
  Actions necessary to create and update Informations
  and set an Information a users profile.
-}
import Data.Maybe
import Happstack.Server as Server

import OpenBrain.Website.Common
import OpenBrain.Website.Session
import qualified OpenBrain.Website.Parameters as Parameters
import qualified OpenBrain.Website.Session    as Session

serve :: OBW Response
serve = msum [
    dir "create"  create
  , dir "update"  update
  , dir "profile" setProfile
  ]

{-
  Parameters:
  title, description, content
-}
create :: OBW Response
create = Session.chkSession' $ \uid -> do
  (title, desc, content) <- Parameters.getTDC
  -- Creating Information:
  let ci = CreateInformation{
      userId        = uid
    , ciTitle       = title
    , ciDescription = desc
    }
  iid <- liftOBB $ AddContentMedia ci content
  jsonSuccess $ "Created Information: " ++ show iid

{-
  Parameters:
  informationId, title, description, content
-}
update :: OBW Response
update = do
  -- Gathering parameters:
  iid <- Parameters.getInformationId
  (title, desc, content) <- Parameters.getTDC
  split <- Parameters.getSplit
  Session.chkSession' $ \uid ->
    handleFail "Could not lookup target " $ do
      i <- liftM fromJust . liftOBB $ GetInformation iid
      let iDeleted = isJust $ deletion i
      handleFail "Information outdated." $ do
        when iDeleted . guard $ split -- iDeleted is only on allowed on split.
        handleFail "Information is not simple Content." $ do
          guard $ isContent $ media i
          handleFail "Could not update " $ do
            iid' <- liftOBB $ UpdateContentMedia uid iid title desc content
            unless split $ liftOBB $ DeleteInformation iid
            jsonSuccess $ "Updated Information: " ++ show iid'

setProfile :: OBW Response
setProfile = do
  iid <- Parameters.getInformationId
  Session.chkSession' $ \uid ->
    handleFail "Error during OpenBrain.Website.Action.Edit:setProfile" $ do
      liftOBB $ SetProfile uid $ Just iid
      jsonSuccess $ "Changed Profilepage: " ++ show iid

