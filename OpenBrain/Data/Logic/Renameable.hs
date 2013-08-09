{-# LANGUAGE FlexibleInstances #-}
module OpenBrain.Data.Logic.Renameable(
  Renameable(..)
) where

import Data.Map (Map)
import qualified Data.Map as Map

class Renameable r where
  rename :: Map String String -> r -> r

instance Renameable String where
  rename m s = case Map.lookup s m of
    (Just s') -> s
    Nothing   -> s

instance (Functor f, Renameable a) => Renameable (f a) where
  rename m = fmap $ rename m
