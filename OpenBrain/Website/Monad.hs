module OpenBrain.Website.Monad (
    WebsiteState(..), OBW
  , runOBW, noMaybe, noMaybes
  , liftOBB, liftMaybe
  , module ControlMonad
  , module ControlMonadState
  , module ControlMonadTrans
  , module ControlMonadTransMaybe
  , module Backend
  , module Config
) where
{-
  Definition of a Website Monad that will help me doing things with more ease.
-}

import Control.Monad                 as ControlMonad
import Control.Monad.State           as ControlMonadState
import Control.Monad.Trans           as ControlMonadTrans
import Control.Monad.Trans.Maybe     as ControlMonadTransMaybe
import Data.Maybe
import Happstack.Server              as Server

import OpenBrain.Backend            as Backend
import OpenBrain.Config             as Config
import qualified OpenBrain.Deadline as Deadline

data WebsiteState = WebsiteState {
    backend       :: CBackend
  , config        :: Config
  , deadlineState :: Deadline.TDState
  }

-- OBW ~ the OpenBrainWebsite Monad
type OBW a = StateT WebsiteState (ServerPartT IO) a

runOBW :: WebsiteState -> OBW a -> ServerPartT IO a
runOBW ws m = evalStateT m ws

-- Filtering OBW results via mzero:
noMaybe :: OBW (Maybe a) -> OBW a
noMaybe m = do
  mVal <- m
  guard  $ isJust mVal
  return $ fromJust mVal

noMaybes :: OBW [Maybe a] -> OBW [a]
noMaybes = liftM catMaybes

-- Some lift operations:
liftOBB :: (LiftBackend b) => b a -> OBW a
liftOBB m = do
  b <- gets backend
  liftIO . process b $ liftB m

liftDeadline :: Deadline.Deadline a -> OBW a
liftDeadline m = liftIO . evalStateT m . deadlineState =<< get

liftMaybe :: Maybe a -> OBW a
liftMaybe = maybe mzero return
