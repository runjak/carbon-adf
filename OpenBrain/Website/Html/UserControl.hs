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
import OpenBrain.Website.Monad
import OpenBrain.Website.Profile
import OpenBrain.Website.Session
import qualified OpenBrain.Data.Profile as P
import qualified OpenBrain.Website.Html.Decorator as Decorator
import qualified OpenBrain.Website.Html.Images as Img

serve :: OBW Response
serve = do
  html <- Decorator.head =<< do
    b <- gets backend
    muid <- lift $ chkSession b
    guard $ isJust muid
    ud <- liftMaybeT $ getUser b $ fromJust muid
    profile <- liftIO $ getProfile b $ userid ud
    return $ do
      editProfile profile
      controlBox $ username ud
    `mplus` return loginBox
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
  (H.fieldset ! A.class_ "editPassword") $ do
    H.label "Change password"
    H.ul $ do
      H.li $ do
        (H.label ! A.for "password") "New"
        H.input ! A.type_ "password" ! A.name "password"
      H.li $ do
        (H.label ! A.for "confirm") "Confirm"
        H.input ! A.type_ "password" ! A.name "confirm"
    H.input ! A.class_ "change" ! A.type_ "button" ! A.value "Change"
  (H.fieldset ! A.class_ "userName") $ do
    H.label "Account"
    H.ul $
      H.li $ do
        (H.label ! A.for "username") "Username"
        H.input ! A.type_ "text" ! A.name "username" ! A.disabled "disabled" ! A.value (H.toValue username)
    H.input ! A.class_ "logout" ! A.type_ "button" ! A.value "Logout"
    H.input ! A.class_ "delete" ! A.type_ "button" ! A.value "Delete"

editProfile :: Profile -> H.Html
editProfile profile = (H.form ! A.acceptCharset "UTF-8" ! A.class_ "editProfile") $ do
  (H.fieldset ! A.class_ "editAvatar") $ do
    H.label "Avatar"
    let avatar = H.toValue . fromMaybe "" $ P.avatar profile
    when (isJust $ P.avatar profile) $ H.img ! A.src avatar
    H.ul $ H.li $ do
      (H.label ! A.for "avatar") "URL to Avatar"
      H.input ! A.type_ "url" ! A.name "avatar" ! A.value avatar
  (H.fieldset ! A.class_ "editName") $ do
    H.label "Username"
    let n = fromMaybe P.emptyName $ P.name profile
    H.ul $ do
      H.li $ do
        (H.label ! A.for "prefix") "Prefix"
        H.input ! A.name "prefix" ! A.value (H.toValue $ P.prefix n)
      H.li $ do
        (H.label ! A.for "foreName") "Forename"
        H.input ! A.name "foreName" ! A.value (H.toValue $ P.foreName n)
      H.li $ do
        (H.label ! A.for "middleName") "Middlename"
        H.input ! A.name "middleName" ! A.value (H.toValue $ P.middleName n)
      H.li $ do
        (H.label ! A.for "familyName") "Familyname"
        H.input ! A.name "familyName" ! A.value (H.toValue $ P.familyName n)
      H.li $ do
        (H.label ! A.for "suffix") "Suffix"
        H.input ! A.name "suffix" ! A.value (H.toValue $ P.suffix n)
  (H.fieldset ! A.class_ "editLocations") $ do
    H.label "Locations"
    let locations = P.locations profile ++ [P.emptyLocation]
    H.ul $ forM_ locations $ \location -> H.li $ do
      toolSet
      (H.ul ! A.class_ "editLocation") $ do
        H.li $ do
          (H.label ! A.for "street") "Street"
          H.input ! A.name "street" ! A.value (H.toValue $ P.street location)
        H.li $ do
          (H.label ! A.for "city") "City"
          H.input ! A.name "city" ! A.value (H.toValue $ P.city location)
        H.li $ do
          (H.label ! A.for "state") "State"
          H.input ! A.name "state" ! A.value (H.toValue $ P.state location)
        H.li $ do
          (H.label ! A.for "land") "Land"
          H.input ! A.name "land" ! A.value (H.toValue $ P.land location)
        H.li $ do
          (H.label ! A.for "zipCode") "ZipCode"
          H.input ! A.name "zipCode" ! A.value (H.toValue $ P.zipCode location)
        H.li $ do
          (H.label ! A.for "note") "Note"
          (H.textarea ! A.name "note") $ H.toHtml $ P.note location
  (H.fieldset ! A.class_ "editWebsites") $ do
    H.label "Websites"
    let websites = P.websites profile ++ [P.emptySnippet]
    H.ul $ forM_ websites $ \website -> H.li $ do
      toolSet
      (H.ul ! A.class_ "editWebsite") $ do
        H.li $ do
          (H.label ! A.for "title") "Title"
          H.input ! A.name "title" ! A.value (H.toValue $ P.title website)
        H.li $ do
          (H.label ! A.for "target") "Website"
          H.input ! A.type_ "url" ! A.name "target" ! A.value (H.toValue $ P.target website)
        H.li $ do
          (H.label ! A.for "description") "Description"
          (H.textarea ! A.name "description") $ H.toHtml $ P.description website
  (H.fieldset ! A.class_ "editEmails") $ do
    H.label "Emails"
    let emails = P.emails profile ++ [P.emptySnippet]
    H.ul $ forM_ emails $ \email -> H.li $ do
      toolSet
      (H.ul ! A.class_ "editEmail") $ do
        H.li $ do
          (H.label ! A.for "title") "Title"
          H.input ! A.name "title" ! A.value (H.toValue $ P.title email)
        H.li $ do
          (H.label ! A.for "target") "Email"
          H.input ! A.type_ "email" ! A.name "target" ! A.value (H.toValue $ P.target email)
        H.li $ do
          (H.label ! A.for "description") "Description"
          (H.textarea ! A.name "description") (H.toHtml $ P.description email)
  (H.fieldset ! A.class_ "editIms") $ do
    H.label "Instant Messagers"
    let ims = P.instantMessagers profile ++ [P.emptySnippet]
    H.ul $ forM_ ims $ \im -> H.li $ do
      toolSet
      (H.ul ! A.class_ "editIm") $ do
        H.li $ do
          (H.label ! A.for "title") "Title"
          H.input ! A.name "title" ! A.value (H.toValue $ P.title im)
        H.li $ do
          (H.label ! A.for "target") "IM"
          H.input ! A.name "target" ! A.value (H.toValue $ P.target im)
        H.li $ do
          (H.label ! A.for "description") "Description"
          (H.textarea ! A.name "description") (H.toHtml $ P.description im)
  H.input ! A.type_ "button" ! A.name "save" ! A.value "Save data"

toolSet :: H.Html
toolSet = H.ul ! A.class_ "toolset" $ do
  H.li ! A.title "Add" $ Img.add
  H.li ! A.title "Clear input" $ Img.clear
  H.li ! A.title "Remove" $ Img.remove

