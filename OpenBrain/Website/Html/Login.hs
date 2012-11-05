{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Login where
{-
  Login/Logout controls
-}

import Data.Maybe
import Happstack.Server as S
import Text.Hastache
import Text.Hastache.Context
import qualified Data.ByteString.Lazy as LZ

import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Website.Monad
import OpenBrain.Website.Template
import qualified OpenBrain.Website.Session as Session

controls :: OBW HTML
controls = do
  mUid <- msum [liftM return Session.chkSession, return Nothing]
  isJust mUid ? (logoutBox $ fromJust mUid, loginBox)

loginBox :: OBW HTML
loginBox = liftIO $ tmpl "LoginBox.html" emptyContext

logoutBox :: UserId -> OBW HTML
logoutBox uid = do
  let context "uid" = MuVariable . show . unwrap $ toId uid
  liftIO $ tmpl "LogoutBox.html" context

