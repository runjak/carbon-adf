{-# LANGUAGE OverloadedStrings#-}
module OpenBrain.Website.Html.User (serve) where
{-
  Displaying single users and lists of users.
  Also forms for registering, login/out and password changing.
-}

import Data.Maybe
import Happstack.Server as S

import OpenBrain.Website.Common
import qualified OpenBrain.Website.Parameters       as Parameters
import qualified OpenBrain.Website.Html.Information as Information
import qualified OpenBrain.Website.Session          as Session

{-
  Listing Users:
    /user.html
    /user.html?limit=_&offset=_
  Displaying a single User:
    /user.html?display=_
-}
serve :: OBW Response
serve = ok =<< msum [serveSingle, serveList]

serveSingle :: OBW Response
serveSingle = do
  uid     <- liftM fromId Parameters.getDisplay
  ud      <- noMaybe . liftOBB $ GetUser uid
  isA     <- msum [Session.chkSession >> return True, return False]
  undefined

serveList :: OBW Response
serveList = do
  limit   <- Parameters.getLimit
  offset  <- Parameters.getOffset
  count   <- liftOBB GetUserCount
  uds     <- liftOBB $ getUsers =<< (liftB $ GetUserList limit offset)
  undefined
