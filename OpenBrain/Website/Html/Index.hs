{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Index (serve) where

import Happstack.Server as S

import OpenBrain.Website.Common
import qualified OpenBrain.Website.Html.Decorator as Decorator

serve :: OBW Response
serve = do
  html <- Decorator.page $ htmlConcat ["This is how we do it again." :: HTML]
  ok. toResponse $ html
