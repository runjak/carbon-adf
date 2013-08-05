module OpenBrain.Data.Logic.NameContainer(
  NameContainer(..)
) where

import Data.List(nub)

class NameContainer n where
  names :: n -> [String]

instance NameContainer n => NameContainer [n] where
  names = nub . concatMap names
