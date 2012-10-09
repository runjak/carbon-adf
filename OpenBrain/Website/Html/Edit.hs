{-# Language OverloadedStrings #-}
module OpenBrain.Website.Html.Edit (editor, serve) where

import Data.Maybe
import Happstack.Server as S
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import qualified OpenBrain.Backend.Monad as OBB
import qualified OpenBrain.Data.Information as I
import qualified OpenBrain.Website.Html.Decorator as Decorator
import qualified OpenBrain.Website.Html.Images as Images

data EditorContent = EditorContent {
    editorTitle :: String
  , title       :: String
  , description :: String
  , content     :: String
  , footerLinks :: [H.Html]
  , iid         :: Maybe InformationId
  }
emptyContent = EditorContent {
    editorTitle = "Create Information"
  , title       = ""
  , description = ""
  , content     = ""
  , footerLinks = [
      H.a ! A.id "EditorSave" $ Images.save' "Save Information" "Save Information"
    , H.a ! A.id "EditorAdd"  $ Images.add' "Add as new Information" "Add as new Information"
    ]
  , iid         = Nothing
  }

{-
  Expected to be one per page for now.
-}
editor :: EditorContent -> H.Html
editor eContent = do
  let viid    = H.toValue . show . unwrap . toId . fromJust $ iid eContent
      addData = isJust (iid eContent) ? (\x -> x ! H.dataAttribute "iid" viid, id)
  addData H.form ! A.id "MarkdownEditor" $ do
  -- Editor headline when given:
  unless (null $ editorTitle eContent) $ H.h1 . H.toHtml $ editorTitle eContent
  -- Input field for the title:
  H.input ! A.id "InformationTitle" ! A.type_ "text" ! A.placeholder "Place title here" ! (A.value . H.toValue $ title eContent)
  -- Textarea for the description:
  H.textarea ! A.id "InformationDescription" ! A.placeholder "Place description here" $ H.toHtml $ description eContent
  -- The content:
  H.label $ do
    "Content:"
    H.textarea ! A.id "InformationContent" ! A.placeholder "Place content here as markdown." $ H.toHtml $ content eContent
  -- Markdown preview:
  H.label $ do
    "Preview:"
    H.div ! A.id "MarkdownPreview" $ ""
  -- Buttons to save content, etc.
  H.ul ! A.id "EditorFooter" $ mapM_ H.li $ footerLinks eContent

mkEditor :: EditorContent -> I.Information -> OBW H.Html
mkEditor ec i = case I.media i of
  (I.Content s) -> do
    let content = ec{
        title       = I.title i
      , description = I.description i
      , content     = s
      , iid         = Just $ I.informationId i
      }
    return $ editor content
  _ -> mzero

{- Serving the editor: -}
serve :: OBW Response
serve = mplus (path lookupInformation) $ do
  p <- Decorator.page $ editor emptyContent
  ok $ toResponse p

lookupInformation :: Id -> OBW Response
lookupInformation i' = handleFail "Can't find requested Information." $ do
  i <- liftOBB . OBB.getInformation $ fromId i'
  handleFail "Information is not simple content - can't edit that." $ do
    p <- Decorator.page =<< mkEditor emptyContent{editorTitle = "Edit Information"} i
    ok $ toResponse p
