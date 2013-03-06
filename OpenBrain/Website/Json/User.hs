{-# LANGUAGE OverloadedStrings#-}
module OpenBrain.Website.Json.User (serve) where
{-
  Displaying single users and lists of users.
  Also forms for registering, login/out and password changing.
-}

import Data.Maybe
import Happstack.Server as S

import OpenBrain.Website.Common
import qualified OpenBrain.Website.Parameters       as Parameters
import qualified OpenBrain.Website.Json.Information as Information
import qualified OpenBrain.Website.Session          as Session

{-
  Listing Users:
    /user
    /user?limit=_&offset=_
  Displaying a single User:
    /user/_
-}
serve :: OBW Response
serve = ok =<< msum [serveSingle, serveList]

serveSingle :: OBW Response
serveSingle = Parameters.withId $ \id -> do
  let uid = fromId id
  ud <- noMaybe . liftOBB $ GetUser uid
  ok $ jsonResponse ud

serveList :: OBW Response
serveList = do
  limit  <- Parameters.getLimit
  offset <- Parameters.getOffset
  count  <- liftOBB GetUserCount
  uds    <- liftOBB $ getUsers =<< (liftB $ GetUserList limit offset)
  ok $ jsonResponse uds
