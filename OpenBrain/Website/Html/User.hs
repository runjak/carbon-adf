{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}
module OpenBrain.Website.Html.User (edit, login, serve) where
{-
  Displaying single users and lists of users.
  Also forms for registering, login/out and password changing.
-}

import Data.Maybe
import Happstack.Server as S
import Text.Blaze ((!))
import Text.Blaze.Html (ToMarkup(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Backend.Types
import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Karma
import OpenBrain.Data.User
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import qualified OpenBrain.Backend.Monad as OBB
import qualified OpenBrain.Website.Html.Decorator as Decorator
import qualified OpenBrain.Website.Html.Information as Information
import qualified OpenBrain.Website.Session as Session

{-
  Login/Logout controls depending on the client.
-}
login :: OBW H.Html
login = do
  mUid <- msum [liftM return $ Session.chkSession, return Nothing]
  if isJust mUid
  then return $ do
    H.form ! A.id "LogoutBox" $ do
      H.button ! A.class_ "Logout" ! A.type_ "button" $ "Logout"
      let uid = H.toValue . show . unwrap . toId $ fromJust mUid
      H.button ! A.class_ "Profile" ! A.type_ "button" ! H.dataAttribute "uid" uid $ "Profile"
  else return $ do
    H.form ! A.id "LoginBox" $ do
      H.label $ do
        "Username:"
        H.input ! A.type_ "text" ! A.name "Username"
      H.label $ do
        "Password:"
        H.input ! A.type_ "password" ! A.name "Password"
      H.button ! A.class_ "Login"   ! A.type_ "button" $ "Login"
      H.button ! A.class_ "Create"  ! A.type_ "button" $ "Create"

{-
  Controls to change the password or delete the user,
  H.Html is empty if there's no session.
  Displays a Username field if client is admin.
-}
edit :: OBW H.Html
edit = flip mplus (return "") $ do
  uid <- Session.chkSession
  ud <- liftOBB $ OBB.getUser uid
  return $ do
    H.form ! A.id "EditBox" $ do
      H.h1 "Update Password:"
      let style = (isAdmin ud) ? (id, \x -> x ! A.style "display: none;")
      style H.label $ do
        "Target Username:"
        H.input ! A.type_ "text" ! A.value (H.toValue $ username ud) ! A.name "Username"
      H.label $ do
        "New password:"
        H.input ! A.type_ "password" ! A.name "Password"
      H.label $ do
        "Confirm:"
        H.input ! A.type_ "confirm" ! A.name "Confirm"
      H.button ! A.class_ "Update" ! A.type_ "button" $ "Update Passwd"
      H.button ! A.class_ "Delete" ! A.type_ "button" $ "Delete user"

{-
  Get parameters to handle:
-}
getDisplay  = liftM fromId $ lookRead "display"   :: OBW UserId
getLimit    = msum [lookRead "limit", return 30]  :: OBW Limit
getOffset   = msum [lookRead "offset", return 0]  :: OBW Offset

{-
  Listing Users:
    /user.html
    /user.html?limit=_&offset=_
  Displaying a single User:
    /user.html?display=_
-}
serve :: OBW Response
serve = ok . toResponse =<< msum [serveSingle, serveList]

serveSingle :: OBW H.Html
serveSingle = do
  uid     <- getDisplay
  ud      <- liftOBB $ OBB.getUser uid
  isA     <- msum [Session.chkSession >> return True, return False]
  profile <- flip mplus (return "") $ do
    guard . isJust $ profile ud
    let iid = fromJust $ profile ud
    i <- liftOBB $ OBB.getInformation iid
    Information.viewSingle i
  Decorator.page $ do
    H.h1 $ "User " >> (H.toHtml $ username ud) >> ":"
    H.dl ! A.class_ "UserData" $ do
      (H.dt ! A.class_ "Karma")    "Karma"    >> (H.dd . H.toHtml . fromKarma $ karma ud)
      (H.dt ! A.class_ "Creation") "Creation" >> (H.dd . H.toHtml $ creation ud)
      when isA $ do
        (H.dt ! A.class_ "LastLogin") "Last login" >> (H.dd . H.toHtml $ lastLogin ud)
    profile

serveList :: OBW H.Html
serveList = do
  limit   <- getLimit
  offset  <- getOffset
  count   <- liftOBB OBB.getUserCount
  uds     <- liftOBB $ OBB.getUsers =<< OBB.getUserList limit offset
  Decorator.page $ do
    H.h1 "Users"
    H.ul ! A.class_ "UserList" $ forM_ uds $ \ud -> H.li $ do
      H.dl ! A.class_ "UserData" $ do
        (H.dt ! A.class_ "Username") "Username"
        let href = (toHref "user.html" ["display=" ++ (show . unwrap . toId $ userid ud)])
        H.dd $ H.a ! A.href href $ H.toHtml $ username ud
        (H.dt ! A.class_ "Karma")    "Karma"    >> (H.dd . H.toHtml . fromKarma $ karma ud)
        (H.dt ! A.class_ "Creation") "Creation" >> (H.dd . H.toHtml $ creation ud)
    H.h2 "Pages:"
    let ps = pages limit offset count
    H.ul ! A.class_ "PageSelection" $ forM_ ps $ \(pageTitle, pageLimit) -> H.li $ do
      H.a ! A.href (toHref "user.html" ["offset=" ++ show offset, "limit=" ++ show pageLimit]) $ H.toHtml pageTitle

