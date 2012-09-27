{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Login where
{-
  Login/Logout controls
-}

import Data.Maybe
import Happstack.Server as S
import Text.Blaze ((!))
import Text.Blaze.Html (ToMarkup(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Website.Monad
import qualified OpenBrain.Website.Html.Images as Images
import qualified OpenBrain.Website.Session as Session

controls :: OBW H.Html
controls = do
  mUid <- msum [liftM return $ Session.chkSession, return Nothing]
  return $ (isJust mUid) ? (logoutBox $ fromJust mUid, loginBox)

logoutBox :: UserId -> H.Html
logoutBox uid' = do
  H.form ! A.id "LoginBox" $ do
    H.button ! A.class_ "Logout" ! A.type_ "button" $ "Logout"
    let uid = H.toValue . show . unwrap $ toId uid'
    H.button ! A.class_ "Profile" ! A.type_ "button" ! H.dataAttribute "uid" uid $ "Profile"
    H.a ! A.class_ "Cancel" $ Images.stop' "Cancel" "Close box."

loginBox :: H.Html
loginBox = do
  H.form ! A.id "LoginBox" $ do
    H.label $ do
      "Username:"
      H.input ! A.id "Username" ! A.type_ "text" ! A.name "Username"
    H.label $ do
      "Password:"
      H.input ! A.id "Password" ! A.type_ "password" ! A.name "Password"
    H.button ! A.class_ "Login"   ! A.type_ "button" $ "Login"
    H.button ! A.class_ "Create"  ! A.type_ "button" $ "Create"
    H.a ! A.class_ "Cancel" $ Images.stop' "Cancel" "Close box."
