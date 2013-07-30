{-# LANGUAGE FlexibleInstances #-}
module OpenBrain.Data.Logic.Renameable(
  Renameable(..)
) where

import Data.Map (Map)

class Renameable r where
  rename :: Map String String -> r -> r

instance (Functor f, Renameable a) => Renameable (f a) where
  rename m = fmap $ rename m
