module OpenBrain.Common(
  liftIOM
, liftIOMay
, (?), powerset, plusm
, module ControlMonad
, module ControlMonadState
, module ControlMonadTrans
, module ControlMonadTransMaybe
)where

import Control.Monad                 as ControlMonad
import Control.Monad.State           as ControlMonadState
import Control.Monad.Trans           as ControlMonadTrans
import Control.Monad.Trans.Maybe     as ControlMonadTransMaybe

liftIOM :: MonadIO m => (a -> b) -> IO a -> m b
liftIOM f = liftIO . liftM f

liftIOMay :: MonadIO m => MaybeT IO a -> m (Maybe a)
liftIOMay = liftIO . runMaybeT

True  ? (a, _) = a
False ? (_, b) = b

{-| O(n²) |-}
powerset :: [a] -> [[a]]
powerset = filterM $ const [True, False]

plusm :: MonadPlus m => m a -> m a -> m a
plusm = flip mplus
