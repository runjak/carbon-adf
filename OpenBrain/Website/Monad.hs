module OpenBrain.Website.Monad where
{-
  Definition of a Website Monad that will help me doing things with more ease.
-}

import Control.Monad
import Control.Monad.State as State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Happstack.Server as Server

import OpenBrain.Backend
import OpenBrain.Config

data WebsiteState = WebsiteState {backend :: CBackend, config :: Config}

-- OBW ~ the OpenBrainWebsite Monad
type OBW a = StateT WebsiteState (ServerPartT IO) a

runOBW :: WebsiteState -> OBW a -> ServerPartT IO a
runOBW ws m = evalStateT m ws

-- Running a MaybeT in OBW using mzero on Nothing.
liftMaybeT :: MaybeT IO a -> OBW a
liftMaybeT m = do
  val <- liftIO $ runMaybeT m
  maybe mzero return val

liftMaybe :: Maybe a -> OBW a
liftMaybe = maybe mzero return

