{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Action (serve) where
{-
  Module for all actions that clients can perform on the website.
  Actions are done via post requests and may change the website content.
  Most of them may require a Session as managed via OpenBrain.Website.Session.
-}

import Control.Monad
import Control.Monad.State
import Happstack.Server as S

import OpenBrain.Backend as B
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import OpenBrain.Website.Session as SM
import qualified OpenBrain.Website.Action.User as User

serve :: OBW Response
serve = do
  let actions = liftM (setHeaderBS "Content-Type" "application/json") $ msum [dir "user" User.serve]
  method POST >> decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
  msum [actions, badRequest "Actions are only allowed via POST requests."]
