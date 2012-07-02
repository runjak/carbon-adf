{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module OpenBrain.Website.Html.User where
{-
  Displaying information regarding a single user to a Client.
-}

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.Maybe
import Data.List (isSuffixOf)
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
import OpenBrain.Data.User
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import OpenBrain.Website.Profile
import OpenBrain.Website.Session
import qualified OpenBrain.Data.Profile as P
import qualified OpenBrain.Website.Html.Decorator as Decorator

showUser :: OBW Response
showUser = path $ \path -> do
  guard $ ".html" `isSuffixOf` path
  let uname = take (length path - 5) path
  b <- gets backend
  c <- gets config
  handleFail ("User " ++ uname ++ " not found.") $ do
    userdata  <- liftMaybeT $ getUserByName b uname
    profile   <- liftMaybeT $ getProfileByUserId b $ userid userdata
    html      <- Decorator.head $ do
      H.toHtml userdata
      H.toHtml profile
    ok $ toResponse html

instance ToMarkup UserData where
  toMarkup ud = (H.dl ! A.class_ "userData") $ do
    (H.dt ! A.class_ "username") "Username" >> (H.dd . H.toHtml $ username ud)
    (H.dt ! A.class_ "karma")    "Karma"    >> (H.dd . H.toHtml . fromKarma $ karma ud)
    (H.dt ! A.class_ "creation") "Creation" >> (H.dd . H.toHtml $ creation ud)

