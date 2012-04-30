module OpenBrain.Website.Action.User (serve) where
{-
  Actions to work with OpenBrain.Backend(UserBackend).
-}

import OpenBrain.Backend (Backend, UserBackend)
import qualified OpenBrain.Backend as B
import OpenBrain.User.Data
import OpenBrain.User.Hash (Hash, hash)
import OpenBrain.Website.Session

import Control.Monad
import Control.Monad.Trans (liftIO)
import Happstack.Server as S

serve :: Backend -> SessionManager -> ServerPartT IO Response
serve b sm = msum [
    dir "create" $ create (B.userBackend b) sm
  , dir "login" $ login (B.userBackend b) sm
  , dir "logout" (dropSession sm >> ok (toResponse "Session dropped."))
  , dir "edit" $ ok (toResponse "not implemented.") -- FIXME define edit -> profiles
  , dir "delete" $ ok (toResponse "not implemented.") -- FIXME introduce roles
  ]

create :: UserBackend -> SessionManager -> ServerPartT IO Response
create ub sm = do
  username  <- look "username"
  hash      <- liftM hash $ look "password"
  mUserData <- liftIO $ B.register ub username hash
  case mUserData of
    Nothing -> badRequest $ toResponse "Fail: Could not register user."
    (Just userdata) -> do
      mkSession sm $ userid userdata
      ok $ toResponse "User created."

login :: UserBackend -> SessionManager -> ServerPartT IO Response
login ub sm = do
  username  <- look "username"
  hash      <- liftM hash $ look "password"
  mUserData <- liftIO $ B.login ub username hash
  case mUserData of
    Nothing -> badRequest $ toResponse "Fail: Login failed."
    (Just userdata) -> do
      let uid = userid userdata
      mkSession sm uid
      ok . toResponse $ show uid

