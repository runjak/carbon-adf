module OpenBrain.Website.Users where
{-
  This module deals with displaying a list of users and their profiles.
  This has to deal with privacy issues.
-}

import Happstack.Server as S
import Text.Blaze ((!))
import Text.Blaze.Html (ToMarkup(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


