{-# Language OverloadedStrings #-}
module OpenBrain.Website.Html.Edit (editor, serve) where

import Happstack.Server as S
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Website.Monad
import qualified OpenBrain.Website.Html.Decorator as Decorator
import qualified OpenBrain.Website.Html.Menu as Menu

editor :: String -> H.Html
editor content = do
  let attrs = [A.id "wysihtml5-textarea"
              ,A.placeholder "Text input…"
              ,A.autofocus "autofocus"]
  H.form $ foldl (!) H.textarea attrs $ H.toHtml content

serve :: OBW Response
serve = do
  p <- Decorator.page $ editor "foo"
  ok $ toResponse p
