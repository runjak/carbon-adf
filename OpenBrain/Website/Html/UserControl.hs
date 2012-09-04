{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module OpenBrain.Website.Html.UserControl where
{-
  Displaying information regarding a single user to a Client.
-}

import Prelude hiding (head)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.Maybe
import Data.Maybe
import Happstack.Server as S
import Text.Blaze ((!))
import Text.Blaze.Html (ToMarkup(..))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Config
import OpenBrain.Common
import OpenBrain.Data.Karma
import OpenBrain.Data.User
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import OpenBrain.Website.Session

import qualified OpenBrain.Backend.Monad as OBB
import qualified OpenBrain.Website.Html.Decorator as Decorator
import qualified OpenBrain.Website.Html.Images as Img

serve :: OBW Response
serve = do
  html  <- Decorator.head =<< do
    uid <- chkSession
    ud  <- liftOBB $ OBB.getUser uid
    return $ do
      controlBox $ username ud
    `mplus` return loginBox
  ok $ toResponse html

loginBox :: H.Html
loginBox = H.form ! A.id "OpenBrainWebsiteUser_loginBox" $ H.ul $ do
  H.li $ do
    (H.label ! A.for "username") "Username"
    H.input ! A.type_ "text" ! A.name "username"
  H.li $ do
    (H.label ! A.for "password") "Password"
    H.input ! A.type_ "password" ! A.name "password"
  H.button ! A.class_ "login" ! A.type_ "button" $ "Login"
  H.button ! A.class_ "create" ! A.type_ "button" $ "Create"

controlBox :: UserName -> H.Html
controlBox username = H.form ! A.id "OpenBrainWebsiteUser_controlBox" $ do
  (H.fieldset ! A.class_ "editPassword") $ do
    H.label "Change password"
    H.ul $ do
      H.li $ do
        (H.label ! A.for "password") "New"
        H.input ! A.type_ "password" ! A.name "password"
      H.li $ do
        (H.label ! A.for "confirm") "Confirm"
        H.input ! A.type_ "password" ! A.name "confirm"
    H.button ! A.class_ "change" ! A.type_ "button" $ "Change"
  (H.fieldset ! A.class_ "userName") $ do
    H.label "Account"
    H.ul $
      H.li $ do
        (H.label ! A.for "username") "Username"
        H.input ! A.type_ "text" ! A.name "username" ! A.disabled "disabled" ! A.value (H.toValue username)
    H.button ! A.class_ "logout" ! A.type_ "button" $ "Logout"
    H.button ! A.class_ "delete" ! A.type_ "button" $ "Delete"

