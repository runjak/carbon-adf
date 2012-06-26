module OpenBrain.Common where

import Control.Monad
import Control.Monad.Trans

liftIOM f = liftIO . liftM f

True  ? (a, _) = a
False ? (_, b) = b
