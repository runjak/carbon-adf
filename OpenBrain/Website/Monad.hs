module OpenBrain.Website.Monad where
{-
  Definition of a Website Monad that will help me doing things with more ease.
-}

import Control.Monad
import Control.Monad.State as State
import Control.Monad.Trans
import Happstack.Server as Server

import OpenBrain.Backend
import OpenBrain.Config

data WebsiteState = WebsiteState {backend :: CBackend, config :: Config}

-- OBW ~ the OpenBrainWebsite Monad
type OBW a = StateT WebsiteState (ServerPartT IO) a

runOBW :: WebsiteState -> OBW a -> ServerPartT IO a
runOBW ws m = evalStateT m ws

