module OpenBrain.Website.Monad (
  module Server
, WebsiteState(..), OBW
, runOBW, noMaybe, noMaybes
, liftB, liftMaybe
, module Backend
, module BackendDSL
, module Config
, module Common
, module Data
) where
{-
  Definition of a Website Monad that will help me doing things with more ease.
-}

import Data.Maybe
import Happstack.Server as Server hiding (port, result)

import OpenBrain.Backend     as Backend
import OpenBrain.Backend.DSL as BackendDSL
import OpenBrain.Config      as Config
import OpenBrain.Common      as Common
import OpenBrain.Data        as Data
import qualified OpenBrain.Deadline as Deadline

data WebsiteState = WebsiteState {
    backend       :: CBackendProcessor
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
liftB :: BackendDSL a -> OBW a
liftB m = do
  b <- gets backend
  liftIO $ process b m

liftDeadline :: Deadline.Deadline a -> OBW a
liftDeadline m = liftIO . evalStateT m . deadlineState =<< get

liftMaybe :: Maybe a -> OBW a
liftMaybe = maybe mzero return
