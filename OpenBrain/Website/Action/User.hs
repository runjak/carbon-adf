{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Action.User (serve) where
{-
  Actions to work with OpenBrain.Backend(UserBackend).
-}

import OpenBrain.Backend (Backend, UserBackend, KarmaBackend)
import qualified OpenBrain.Backend as B
import OpenBrain.User.Data
import OpenBrain.User.Hash (Hash, hash)
import OpenBrain.Website.Action.Common (jToResponse)
import OpenBrain.Website.Common
import OpenBrain.Website.Session

import Data.Aeson
import Control.Monad
import Control.Monad.Trans (liftIO)
import Happstack.Server as S

serve :: Backend -> SessionManager -> ServerPartT IO Response
serve b sm = msum [
    dir "create"  $ create (B.userBackend b) sm
  , dir "login"   $ login (B.userBackend b) sm
  , dir "logout"  $ logout sm
  , dir "edit"    $ edit sm b
  , dir "delete"  $ ok "not implemented." -- FIXME no roles, only karma and admins and self.
  ]

create :: UserBackend -> SessionManager -> ServerPartT IO Response
create ub sm = do
  username  <- look "username"
  hash      <- liftM hash $ look "password"
  mUserData <- liftIO $ B.register ub username hash
  case mUserData of
    Nothing -> badRequest . jToResponse $ object [
        "message" .= ("Fail: Could not register user." :: String)
      , "success" .= False
      ]
    (Just userdata) -> do
      mkSession sm $ userid userdata
      ok . jToResponse $ object [
          "message" .= ("User created." :: String)
        , "success" .= True
        ]

login :: UserBackend -> SessionManager -> ServerPartT IO Response
login ub sm = do
  username  <- look "username"
  hash      <- liftM hash $ look "password"
  mUserData <- liftIO $ B.login ub username hash
  case mUserData of
    Nothing -> badRequest . jToResponse $ object [
        "message" .= ("Fail: Login failed." :: String)
      , "success" .= False
      ]
    (Just userdata) -> do
      let uid = userid userdata
      mkSession sm uid
      ok . jToResponse $ object [
          "message" .= ("Login complete" :: String)
        , "success" .= True
        , "uid"     .= uid
        ]

logout :: SessionManager -> ServerPartT IO Response
logout sm = do
  dropSession sm
  ok . jToResponse $ object [
      "message" .= ("Logout complete" :: String)
    , "success" .= True
    ]

-- we need profiles for this
edit :: SessionManager -> Backend -> ServerPartT IO Response
edit sm b = ok "not implemented - need to define profiles first." {- do
  uid   <- liftM userId $ chkAction sm
  mUser <- B.getUser uid $ B.userBackend b
  case mUser of
    Nothing -> badRequest . jToResponse $ object [
        "message" .= ("Fail: User not found." :: String)
      , "success" .= False
      ]
    (Just user) -> do
      kMustHave <- B.karmaEditUser $ B.karmaBackend b
      if (karma user)-}

delete :: SessionManager -> Backend -> ServerPartT IO Response
delete sm b = do
  uid <- liftM userId $ chkAction sm
  ok "not implemented - do so!"
