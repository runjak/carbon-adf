{-# LANGUAGE FlexibleInstances #-}
module OpenBrain.Website.Monad (
  module Server
, WebsiteState(..), OBW
, runOBW
, Task, fork
, BackendContainer(..)
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
import qualified Control.Concurrent as Concurrent

import OpenBrain.Backend     as Backend
import OpenBrain.Backend.DSL as BackendDSL
import OpenBrain.Config      as Config
import OpenBrain.Common      as Common
import OpenBrain.Data        as Data

data WebsiteState = WebsiteState {
    backend       :: CBackendProcessor
  , config        :: Config
  }

-- OBW ~ the OpenBrainWebsite Monad
type OBW a = StateT WebsiteState (ServerPartT IO) a

runOBW :: WebsiteState -> OBW a -> ServerPartT IO a
runOBW ws m = evalStateT m ws

-- executing something in parallel:
type Task a = StateT WebsiteState IO a

fork :: Task () -> OBW ()
fork t = liftIO . void . runStateT t =<< get

-- Things that allow execution of BackendDSL:
class MonadIO m => BackendContainer m where
  liftB :: BackendDSL a -> m a

-- | Instance for OBW
instance BackendContainer (StateT WebsiteState (ServerPartT IO)) where
  liftB m = do
    b <- gets backend
    liftIO $ process b m

-- | Instance for Task
instance BackendContainer (StateT WebsiteState IO) where
  liftB m = do
    b <- gets backend
    liftIO $ process b m
