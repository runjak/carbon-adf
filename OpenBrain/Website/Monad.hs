module OpenBrain.Website.Monad (
    WebsiteState(..), OBW
  , runOBW, liftOBB
  , liftMaybeT, liftMaybe
  , module ControlMonad
  , module ControlMonadState
  , module ControlMonadTrans
  , module ControlMonadTransMaybe
) where
{-
  Definition of a Website Monad that will help me doing things with more ease.
-}

import Control.Monad              as ControlMonad
import Control.Monad.State        as ControlMonadState
import Control.Monad.Trans        as ControlMonadTrans
import Control.Monad.Trans.Maybe  as ControlMonadTransMaybe
import Happstack.Server as Server
import qualified Control.Monad.Error as Error

import OpenBrain.Backend
import OpenBrain.Backend.Monad
import OpenBrain.Config

data WebsiteState = WebsiteState {backend :: Backend, config :: Config}

-- OBW ~ the OpenBrainWebsite Monad
type OBW a = StateT WebsiteState (ServerPartT IO) a

runOBW :: WebsiteState -> OBW a -> ServerPartT IO a
runOBW ws m = evalStateT m ws

liftOBB :: OBB a -> OBW a
liftOBB m = do
  b <- gets backend
  liftMaybeT $ evalStateT m b

-- Running a MaybeT in OBW using mzero on Nothing.
liftMaybeT :: MaybeT IO a -> OBW a
liftMaybeT m = do
  val <- liftIO $ runMaybeT m
  maybe mzero return val

liftMaybe :: Maybe a -> OBW a
liftMaybe = maybe mzero return

