{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module OpenBrain.Website.User (userControl, userList) where
{-
  Displaying information regarding a single user to a Client.
  Narf - we need some guildelines here to ensure data safety and ++privacy
-}

import Control.Monad
import Control.Monad.Trans(liftIO)
import Data.Maybe
import Happstack.Server as S
import Text.Blaze ((!))
import Text.Blaze.Html (ToMarkup(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Backend
import OpenBrain.Data.Karma
import OpenBrain.Data.Profile (Profile, AccessRule, Name, Location, ProfileSnippet)
import qualified OpenBrain.Data.Profile as P
import OpenBrain.Data.User
import OpenBrain.Website.Session

userControl :: Backend -> ServerPartT IO H.Html
userControl b = do
  muid <- chkSession b
  guard $ isJust muid
  ud <- liftIO . liftM (fromJust) $ getUser (userBackend b) (fromJust muid)
  return . controlBox $ username ud
  `mplus` (return loginBox)

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

userList :: Backend -> ServerPart H.Html
userList b = do
  userdata <- liftIO $ do
    uids   <- getUserList $ userBackend b
    mud    <- mapM (getUser $ userBackend b) uids
    return $ catMaybes mud
  return $ H.ul ! A.class_ "userList" $ do
    flip mapM_ userdata $
      \ud -> H.li ! A.class_ "user" $ do
        when (isAdmin ud) "Admin: "
        H.toHtml $ username ud
        H.toHtml $ " (" ++ show (fromKarma $ karma ud) ++ ")"

