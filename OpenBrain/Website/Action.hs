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

import OpenBrain.Website.Common
import OpenBrain.Website.Session
import qualified OpenBrain.Website.Action.Discussion as Discussion
import qualified OpenBrain.Website.Action.Edit as Edit
import qualified OpenBrain.Website.Action.Relation as Relation
import qualified OpenBrain.Website.Action.User as User

serve :: OBW Response
serve = do -- actions liftM commented to enable different answers than json
  let actions = {-liftM (setHeaderBS "Content-Type" "application/json") $-} msum [
                dir "discussion" Discussion.serve
              , dir "edit"       Edit.serve
              , dir "relation"   Relation.serve
              , dir "user"       User.serve
              ]
  --method POST FIXME uncomment to allow only post requests
  decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
  msum [actions, badRequest "Actions are only allowed via POST requests."]
