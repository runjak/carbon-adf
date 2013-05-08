{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Common(
  module Crud
, module Monad
)where
{-
  Stuff to be included in various OpenBrain.Website modules.
-}

import Data.String (IsString(..))
import Happstack.Server as S

import OpenBrain.Website.Crud  as Crud 
import OpenBrain.Website.Monad as Monad

-- For fun with OverloadedStrings pragma
instance IsString Response where
  fromString = toResponse

responseHTML :: Response -> Response
responseHTML = setHeader "Content-Type" "text/html"

responseJSON :: Response -> Response
responseJSON = setHeader "Content-Type" "application/json"

