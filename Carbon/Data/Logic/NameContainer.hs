module Carbon.Data.Logic.NameContainer(
  NameContainer(..)
) where

import Data.Set(Set)
import qualified Data.Set as Set

class NameContainer n where
  names :: n -> Set String

instance NameContainer n => NameContainer [n] where
  names = Set.unions . map names
