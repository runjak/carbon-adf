module Carbon.Common(
  (?), powerset, powerset', plusm
, module ControlMonad
, module ControlMonadState
, module ControlMonadTrans
, module ControlMonadTransMaybe
)where

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad             as ControlMonad
import Control.Monad.State       as ControlMonadState
import Control.Monad.Trans       as ControlMonadTrans
import Control.Monad.Trans.Maybe as ControlMonadTransMaybe

True  ? (a, _) = a
False ? (_, b) = b

{-|
  O(n²)
|-}
powerset :: [a] -> [[a]]
powerset = filterM $ const [True, False]

powerset' :: Ord a => Set a -> [Set a]
powerset' = map Set.fromList . powerset . Set.toList

plusm :: MonadPlus m => m a -> m a -> m a
plusm = flip mplus
