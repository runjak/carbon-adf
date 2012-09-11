{-# Language OverloadedStrings #-}
module OpenBrain.Website.Html.Menu (menu) where

import Happstack.Server as S
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Website.Monad

menu :: OBW H.Html
menu = return menu'

menu' :: H.Html
menu' = H.div ! A.id "menu" $ do
  editorToolbar

{- Code that wysihtml5 uses as a toolbar -}
editorToolbar :: H.Html
editorToolbar = H.div ! A.id "EditorToolbar" ! A.style "display: none;" $ do
  H.a ! (H.dataAttribute "wysihtml5-command" "bold") $ "bold"
  H.a ! (H.dataAttribute "wysihtml5-command" "italic") $ "italic"
  H.a ! (H.dataAttribute "wysihtml5-command" "createLink") $ "insert link"
  H.a ! (H.dataAttribute "wysihtml5-command" "insertImage") $ "insert image"
  H.a ! (H.dataAttribute "wysihtml5-command" "insertUnorderedList") $ "insertUnorderedList"
  H.a ! (H.dataAttribute "wysihtml5-command" "insertOrderedList") $ "insertOrderedList"
  H.a ! (H.dataAttribute "wysihtml5-command" "formatBlock") ! (H.dataAttribute "wysihtml5-command-value" "h1") $ "h1"
  H.a ! (H.dataAttribute "wysihtml5-command" "formatBlock") ! (H.dataAttribute "wysihtml5-command-value" "h2") $ "h2"
  H.a ! (H.dataAttribute "wysihtml5-command" "change_view") $ "change view"
  -- Dialog for link creation:
  H.div ! (H.dataAttribute "wysihtml5-dialog" "createLink") ! A.style "display: none;" $ do
    H.label $ do
      "Link:"
      H.input ! (H.dataAttribute "wysihtml5-dialog-field" "href") ! A.value "http://" ! A.class_ "text"
    H.a ! (H.dataAttribute "wysihtml5-dialog-action" "save") $ "OK"
    H.a ! (H.dataAttribute "wysihtml5-dialog-action" "cancel") $ "Cancel"
  -- Dialog for image insertion:
  H.div ! (H.dataAttribute "wysihtml5-dialog" "insertImage") ! A.style "display: none;" $ do
    H.label $ do
      "Image:"
      H.input ! (H.dataAttribute "wyssihtml5-dialog-field" "src") ! A.value "http://"
    H.label $ do
      "Align:"
      H.select ! (H.dataAttribute "wysihtml5-dialog-field" "className") $ do
        H.option ! A.value "" $ "default"
        H.option ! A.value "img-left" $ "left"
        H.option ! A.value "img-right" $ "right"
    H.a ! (H.dataAttribute "wysihtml5-dialog-action" "save") $ "OK"
    H.a ! (H.dataAttribute "wysihtml5-dialog-action" "cancel") $ "Cancel"

