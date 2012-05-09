{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Action (serve) where
{-
  Module for all actions that clients can perform on the website.
  Actions are done via post requests and may change the website content.
  Most of them may require a Session as managed via OpenBrain.Website.Session.
-}

import OpenBrain.Backend as B
import OpenBrain.Website.Common
import OpenBrain.Website.Session as SM
import qualified OpenBrain.Website.Action.User as User

import Control.Monad
import Happstack.Server as S

serve :: Backend -> SessionManager -> ServerPartT IO Response
serve backend sessionmanager = msum [
    method POST >> dir "user" (User.serve backend sessionmanager)
  , badRequest "Actions are only allowed via POST requests."
  ]
