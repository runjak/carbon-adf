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
import qualified OpenBrain.Website.Session as Session

serve :: OBW Response
serve = msum [
    dir "create" create
  , dir "update" update
  ]

{-
  Parameters:
  title, description, content
-}
create :: OBW Response
create = do
  -- Gathering parameters:
  uid     <- Session.chkSession
  title   <- getTitle
  desc    <- getDescription
  content <- getContent
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
  iid     <- getInformationId
  title   <- getTitle
  desc    <- getDescription
  content <- getContent
  split   <- getSplit
  handleFail "Login required" $ do
    uid     <- Session.chkSession
    -- Fetching initial Information to compare:
    handleFail "Could not lookup target Information." $ do
      i <- liftOBB $ OBB.getInformation iid
      let iDeleted = isJust $ Information.deletion i
      handleFail "Information outdated." $ do
        unless split . guard $ not iDeleted
        handleFail "Information is not simple Content." $ do
          guard $ Information.isContent $ Information.media i
          handleFail "Could not update Information." $ do
            iid' <- liftOBB $ OBB.updateContentMedia uid iid title desc content
            unless split $ liftOBB $ OBB.deleteInformation iid
            handleSuccess $ "Updated Information: " ++ show iid'

{- Functions to help with the deal: -}
getInformationId  = liftM fromId $ lookRead "informationId"       :: OBW InformationId
getTitle          = look "title"                                  :: OBW Title
getDescription    = look "description"                            :: OBW Description
getContent        = liftM sanitize $ look "content"               :: OBW Content
getSplit          = flip mplus (return False) $ lookRead "split"  :: OBW Bool

sanitize :: String -> String
sanitize = foldl1 (.) [
    \x -> subRegex (mkRegex "<") x "&lt;"
  , \x -> subRegex (mkRegex ">") x "&gt;"
  ]

