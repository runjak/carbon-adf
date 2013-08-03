module OpenBrain.Common(
  (?), powerset, plusm
, module ControlMonad
, module ControlMonadState
, module ControlMonadTrans
, module ControlMonadTransMaybe
)where

import Control.Monad                 as ControlMonad
import Control.Monad.State           as ControlMonadState
import Control.Monad.Trans           as ControlMonadTrans
import Control.Monad.Trans.Maybe     as ControlMonadTransMaybe

True  ? (a, _) = a
False ? (_, b) = b

{-|
  O(n²)
  I don't need this one at the moment,
  but I also don't want to throw it out,
  because I like the code.
|-}
powerset :: [a] -> [[a]]
powerset = filterM $ const [True, False]

plusm :: MonadPlus m => m a -> m a -> m a
plusm = flip mplus
