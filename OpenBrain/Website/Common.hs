module OpenBrain.Website.Common where
{-
  Stuff to be included in various OpenBrain.Website modules.
-}

import Data.String (IsString(..))
import Happstack.Server

-- For fun with OverloadedStrings pragma
instance IsString Response where
  fromString = toResponse
