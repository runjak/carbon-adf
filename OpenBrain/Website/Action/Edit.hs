module OpenBrain.Website.Action.Edit (serve) where
{-
  Actions necessary for OpenBrain.Website.Action.Edit
-}
import Data.Maybe
import Happstack.Server as Server
import Text.Regex as T

import OpenBrain.Backend.Types
import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import OpenBrain.Website.Session
import qualified OpenBrain.Backend.Monad as OBB
import qualified OpenBrain.Data.Information as Information
import qualified OpenBrain.Website.Parameters as Parameters
import qualified OpenBrain.Website.Session as Session

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
create = do
  -- Gathering parameters:
  uid     <- Session.chkSession
  (title, desc, content) <- Parameters.getTDC
  -- Creating Information:
  let ci = CreateInformation{
      userId      = uid
    , title       = title
    , description = desc
    }
  iid <- liftOBB $ OBB.addContentMedia ci content
  handleSuccess $ "Created Information: " ++ show iid

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
  handleFail "Login required" $ do
    uid <- Session.chkSession
    handleFail "Could not lookup target Information." $ do
      i <- liftOBB $ OBB.getInformation iid
      let iDeleted = isJust $ Information.deletion i
      handleFail "Information outdated." $ do
        when iDeleted . guard $ split -- iDeleted is only on allowed on split.
        handleFail "Information is not simple Content." $ do
          guard $ Information.isContent $ Information.media i
          handleFail "Could not update Information." $ do
            iid' <- liftOBB $ OBB.updateContentMedia uid iid title desc content
            unless split $ liftOBB $ OBB.deleteInformation iid
            handleSuccess $ "Updated Information: " ++ show iid'

setProfile :: OBW Response
setProfile = do
  iid <- Parameters.getInformationId
  handleFail "Login required" $ do
    uid <- Session.chkSession
    handleFail "Error during OpenBrain.Website.Action.Edit:setProfile" $ do
      liftOBB $ OBB.setProfile uid $ Just iid
      handleSuccess $ "Changed Profilepage: " ++ show iid

