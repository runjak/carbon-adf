{-# LANGUAGE DoAndIfThenElse, OverloadedStrings #-}
module OpenBrain.Website.Html.Login (controls) where
{-
  Login/Logout controls
-}

import Data.Maybe
import Happstack.Server as S
import Text.Blaze ((!))
import Text.Blaze.Html (ToMarkup(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Data.Id
import OpenBrain.Website.Monad
import qualified OpenBrain.Website.Session as Session

controls :: OBW H.Html
controls = do
  mUid <- msum [liftM return $ Session.chkSession, return Nothing]
  if isJust mUid
  then return $ do
    H.form ! A.id "LoginBox" $ do
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
