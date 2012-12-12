module OpenBrain.Common where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

liftIOM :: MonadIO m => (a -> b) -> IO a -> m b
liftIOM f = liftIO . liftM f

liftIOMay :: MonadIO m => MaybeT IO a -> m (Maybe a)
liftIOMay = liftIO . runMaybeT

True  ? (a, _) = a
False ? (_, b) = b

{-| O(n²) |-}
powerset :: [a] -> [[a]]
powerset = filterM $ const [True, False]
