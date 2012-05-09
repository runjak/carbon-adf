{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Action.Common (jToResponse) where
{-
  Things to be used across modules of OpenBrain.Website.Action.*
-}

import Data.Aeson
import Happstack.Server

jToResponse :: (ToJSON j) => j -> Response
jToResponse = toResponse .  encode
