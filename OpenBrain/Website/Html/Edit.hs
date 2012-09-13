{-# Language OverloadedStrings #-}
module OpenBrain.Website.Html.Edit (editor, serve) where

import Happstack.Server as S
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Website.Monad
import qualified OpenBrain.Website.Html.Decorator as Decorator
import qualified OpenBrain.Website.Html.Menu as Menu

data EditorContent = EditorContent {
    editorTitle :: String
  , title       :: String
  , description :: String
  , content     :: String
  , footerLinks :: [H.Html]
  }
emptyContent = EditorContent {
    editorTitle = "Create Information"
  , title       = ""
  , description = ""
  , content     = "Put some content here."
  , footerLinks = ["foo","bar"]
  }

{-
  Expected to be one per page for now.
-}
editor :: EditorContent -> H.Html
editor eContent = H.form ! A.id "editor" $ do
  -- Editor headline when given:
  when (not . null $ editorTitle eContent) $ H.h1 . H.toHtml $ editorTitle eContent
  H.section $ do
    -- Input field for the title:
    H.input ! A.id "InformationTitle" ! A.type_ "text" ! A.placeholder "Place title here" ! (A.value . H.toValue $ title eContent)
    -- Textarea for the description:
    H.textarea ! A.id "InformationDescription" ! A.placeholder "Place description here" $ H.toHtml $ description eContent
  -- Toolbar of the editor:
  let dCommand      = \c -> H.dataAttribute "wysihtml5-command"       c
      dCommandValue = \v -> H.dataAttribute "wysihtml5-command-value" v
      dDialog       = \d -> H.dataAttribute "wysihtml5-dialog"        d
      dDialogAction = \a -> H.dataAttribute "wysihtml5-dialog-action" a
      dDialogField  = \f -> H.dataAttribute "wysihtml5-dialog-field"  f
  H.header $ H.ul ! A.id "EditorToolbar" ! A.style "display: none;" $ do
    H.li ! dCommand "bold"                              $ "bold"
    H.li ! dCommand "italic"                            $ "italic"
    H.li ! dCommand "createLink"                        $ "link"
    H.li ! dCommand "insertImage"                       $ "image"
    H.li ! dCommand "insertUnorderedList"               $ "ul"
    H.li ! dCommand "insertOrderedList"                 $ "ol"
    H.li ! dCommand "formatBlock" ! dCommandValue "h1"  $ "h1"
    H.li ! dCommand "formatBlock" ! dCommandValue "h2"  $ "h2"
    H.li ! dCommand "change_view"                       $ "html"
    -- Dialog for link creation:
    H.div ! dDialog "createLink" ! A.style "display: none;" $ do
      H.label $ do
        "Link:"
        H.input ! dDialogField "href" ! A.value "http://" ! A.class_ "text"
      H.button ! dDialogAction "save"    $ "OK"
      H.button ! dDialogAction "cancel"  $ "Cancel"
    -- Dialog for image insertion:
    H.div ! dDialog "insertImage" ! A.style "display: none;" $ do
      H.label $ do
        "Image:"
        H.input ! dDialogField "src" ! A.value "http://"
      H.label $ do
        "Align:"
        H.select ! dDialogField "className" $ do
          H.option ! A.value ""           $ "default"
          H.option ! A.value "img-left"   $ "left"
          H.option ! A.value "img-right"  $ "right"
      H.button ! dDialogAction "save"   $ "OK"
      H.button ! dDialogAction "cancel" $ "Cancel"
  -- The content:
  H.section $ do
    let attrs = [A.id "wysihtml5-textarea"
                ,A.placeholder ""
                ,A.autofocus "autofocus"]
    foldl (!) H.textarea attrs $ H.toHtml $ content eContent
  -- Buttons to save content, etc.
  H.footer $ H.ul ! A.id "EditorFooter" $ mapM_ H.li $ footerLinks eContent

serve :: OBW Response
serve = do
  p <- Decorator.page $ editor emptyContent
  ok $ toResponse p
