{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Index (serve) where

import Happstack.Server as S
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Website.Monad
import qualified OpenBrain.Website.Html.Decorator as Decorator

serve :: OBW Response
serve = do
  html <- Decorator.head $ H.p "This is how we do it again."
  ok. toResponse $ html
