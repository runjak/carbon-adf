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

import OpenBrain.Backend
import OpenBrain.Backend.Plus
import OpenBrain.Config
import OpenBrain.Common
import OpenBrain.Data.Karma
import OpenBrain.Data.Profile (Profile, AccessRule, Name, Location, ProfileSnippet)
import OpenBrain.Data.User
import OpenBrain.Website.Common
import OpenBrain.Website.Index (head)
import OpenBrain.Website.Monad
import OpenBrain.Website.Profile
import OpenBrain.Website.Session
import qualified OpenBrain.Data.Profile as P
import qualified OpenBrain.Website.Html.Decorator as Decorator

serve :: OBW Response
serve = do
  html <- Decorator.head =<< do
    b   <- gets backend
    muid <- lift $ chkSession b
    guard $ isJust muid
    ud  <- liftMaybeT $ getUser b $ fromJust muid
    return . controlBox $ username ud
    `mplus` (return loginBox)
  ok $ toResponse html

loginBox :: H.Html
loginBox = H.form ! A.id "OpenBrainWebsiteUser_loginBox" $ do
  "Username:"
  H.input ! A.type_ "text" ! A.name "username"
  H.br
  "Password:"
  H.input ! A.type_ "password" ! A.name "password"
  H.br
  H.input ! A.class_ "login" ! A.type_ "button" ! A.value "Login"
  H.input ! A.class_ "create" ! A.type_ "button" ! A.value "Create"

controlBox :: UserName -> H.Html
controlBox username = H.form ! A.id "OpenBrainWebsiteUser_controlBox" $ do
  "Username:"
  H.input ! A.type_ "username" ! A.name "username" ! A.disabled "disabled" ! A.value (H.toValue username)
  H.br
  H.input ! A.class_ "logout" ! A.type_ "button" ! A.value "Logout"
  H.input ! A.class_ "delete" ! A.type_ "button" ! A.value "Delete"
  H.br
  "New password:"
  H.input ! A.type_ "password" ! A.name "password"
  H.br
  "Confirm password:"
  H.input ! A.type_ "password" ! A.name "confirm"
  H.br
  H.input ! A.class_ "change" ! A.type_ "button" ! A.value "Change"

