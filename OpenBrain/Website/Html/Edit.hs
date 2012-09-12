{-# Language OverloadedStrings #-}
module OpenBrain.Website.Html.Edit (editor, serve) where

import Happstack.Server as S
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Website.Monad
import qualified OpenBrain.Website.Html.Decorator as Decorator
import qualified OpenBrain.Website.Html.Menu as Menu

{-
  Expected to be one per page for now.
-}
editor :: String -> H.Html
editor content = H.form ! A.id "editor" $ do
  -- Toolbar of the editor:
  let dCommand      = \c -> H.dataAttribute "wysihtml5-command" c
      dCommandValue = \v -> H.dataAttribute "wysihtml5-command-value" v
  H.header $ H.ul ! A.id "EditorToolbar" ! A.style "display: none;" $ do
    H.li ! dCommand "bold"                              $ "bold"
    H.li ! dCommand "italic"                            $ "italic"
    H.li ! dCommand "createLink"                        $ "link"
    H.li ! dCommand "insertImage"                       $ "image"
    H.li ! dCommand "insertUnorderedList"               $ "UnorderedList"
    H.li ! dCommand "insertOrderedList"                 $ "OrderedList"
    H.li ! dCommand "formatBlock" ! dCommandValue "h1"  $ "h1"
    H.li ! dCommand "formatBlock" ! dCommandValue "h2"  $ "h2"
    H.li ! dCommand "change_view"                       $ "change view"
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
  -- The content:
  H.section $ do
    let attrs = [A.id "wysihtml5-textarea"
                ,A.placeholder "Text input…"
                ,A.autofocus "autofocus"]
    foldl (!) H.textarea attrs $ H.toHtml content
  -- Buttons to save content, etc.
  H.footer $ do
    "Save buttons and thelike."
    H.a ! A.id "resize" $ "resize"

serve :: OBW Response
serve = do
  p <- Decorator.page $ editor "foo"
  ok $ toResponse p
