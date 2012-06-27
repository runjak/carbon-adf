{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module OpenBrain.Website.User (serve, userControl, userList) where
{-
  Displaying information regarding a single user to a Client.
-}

import Prelude hiding (head)
import Control.Monad
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
import qualified OpenBrain.Data.Profile as P
import OpenBrain.Data.User
import OpenBrain.Website.Common
import OpenBrain.Website.Index (head)
import OpenBrain.Website.Profile
import OpenBrain.Website.Session

serve :: CBackend -> Config -> ServerPart Response
serve b c = mplus (showUser b c) $ do
  uc <- userControl b
  ul <- userList b
  ok . toResponse . H.docTypeHtml $ do
  head c
  H.body $ do
  uc
  ul

showUser :: CBackend -> Config -> ServerPart Response
showUser b c = path $ \uname -> do
  mud <- liftIOMay $ getUserByName (userBackend b) uname
  case mud of
    Nothing -> badRequest . toResponse $ "User " ++ uname ++ " not found."
    (Just ud) -> do
      mprofile <- liftIO $ do
        pid <- getProfileId (profileBackend $ userBackend b) (userid ud)
        runMaybeT $ getProfile (profileBackend $ userBackend b) pid
      ok . toResponse $ do
        H.docTypeHtml $ do
          head c
          H.body $ do
            H.toHtml ud
            when (isJust mprofile) $ H.toHtml (fromJust mprofile)

userControl :: CBackend -> ServerPart H.Html
userControl b = do
  muid <- chkSession b
  guard $ isJust muid
  ud <- liftM fromJust . liftIOMay $ getUser (userBackend b) (fromJust muid)
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

userList :: CBackend -> ServerPart H.Html
userList b = do
  userdata <- liftIO $ getUserDataList (userBackend b) 0 100
  return $ H.ul ! A.class_ "userList" $ do
    flip mapM_ userdata $
      \ud -> H.li ! A.class_ "user" $ do
        when (isAdmin ud) "Admin: "
        (H.a ! A.href (H.toValue . ("user/"++) $ username ud)) $ do
          H.toHtml $ username ud
          H.toHtml $ " (" ++ show (fromKarma $ karma ud) ++ ")"

instance ToMarkup UserData where
  toMarkup ud = (H.dl ! A.class_ "userData") $ do
    (H.dt ! A.class_ "username") "Username" >> (H.dd . H.toHtml $ username ud)
    (H.dt ! A.class_ "karma")    "Karma"    >> (H.dd . H.toHtml . fromKarma $ karma ud)
    (H.dt ! A.class_ "creation") "Creation" >> (H.dd . H.toHtml $ creation ud)

