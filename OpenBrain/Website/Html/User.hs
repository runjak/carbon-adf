{-# LANGUAGE OverloadedStrings#-}
module OpenBrain.Website.Html.User (edit, serve) where
{-
  Displaying single users and lists of users.
  Also forms for registering, login/out and password changing.
-}

import Data.Maybe
import Happstack.Server as S
import Text.Hastache
import Text.Hastache.Context

import OpenBrain.Backend.Types
import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Karma
import OpenBrain.Data.User
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import OpenBrain.Website.Template
import qualified OpenBrain.Backend.Monad as OBB
import qualified OpenBrain.Website.Parameters as Parameters
import qualified OpenBrain.Website.Html.Decorator as Decorator
import qualified OpenBrain.Website.Html.Information as Information
import qualified OpenBrain.Website.Session as Session

edit :: UserId -> OBW HTML
edit target = flip mplus (return "") $ do
  uid   <- Session.chkSession
  admin <- liftM isAdmin $ liftOBB $ OBB.getUser uid
  ud    <- liftOBB $ OBB.getUser target
  guard $ admin || (uid == target)
  let context "username" = MuVariable $ username ud
  liftIO $ tmpl "UserEdit.html" context

{-
  Listing Users:
    /user.html
    /user.html?limit=_&offset=_
  Displaying a single User:
    /user.html?display=_
-}
serve :: OBW Response
serve = ok . toResponse =<< msum [serveSingle, serveList]

serveSingle :: OBW HTML
serveSingle = do
  uid     <- liftM fromId Parameters.getDisplay
  ud      <- liftOBB $ OBB.getUser uid
  isA     <- msum [Session.chkSession >> return True, return False]
  editBox <- edit uid
  profile <- flip mplus (return "") $ do
    guard . isJust $ profile ud
    let iid = fromJust $ profile ud
    i <- liftOBB $ OBB.getInformation iid
    Information.viewSingle i
  let context "Username"  = MuVariable $ username ud
      context "Karma"     = MuVariable . fromKarma $ karma ud
      context "Creation"  = MuVariable . show $ creation ud
      context "IsAdmin"   = MuBool isA
      context "LastLogin" = MuVariable . show $ lastLogin ud
  description <- liftIO $ tmpl "UserDescription.html" context
  Decorator.page $ htmlConcat [description, editBox, profile]

serveList :: OBW HTML
serveList = do
  limit   <- Parameters.getLimit
  offset  <- Parameters.getOffset
  count   <- liftOBB OBB.getUserCount
  uds     <- liftOBB $ OBB.getUsers =<< OBB.getUserList limit offset
  let context "ListItems" = MuList $ map (mkStrContext . userContext) uds
  uList   <- liftIO $ tmpl "UserList.html" context
  pages   <- pages' limit offset count
  Decorator.page $ htmlConcat [uList, pages]
  where
    userContext ud "DisplayLink" = MuVariable . ("user.html?display="++)
                                 . show . unwrap . toId $ userid ud
    userContext ud "Username"    = MuVariable $ username ud
    userContext ud "Karma"       = MuVariable . fromKarma $ karma ud
    userContext ud "Creation"    = MuVariable . show $ creation ud

pages' :: Limit -> Offset -> Count -> OBW HTML
pages' l o c = do
  let ps = pages l o c
      context "Pages" = MuList $ map (mkStrContext . pageContext) ps
  liftIO $ tmpl "Pages.html" context
  where
    pageContext (title, _) "PageTitle" = MuVariable title
    pageContext (_, limit) "PageLink"  = MuVariable $ "user.html?offset=" ++ show o ++ "&limit=" ++ show limit

