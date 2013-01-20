{-# Language OverloadedStrings #-}
module OpenBrain.Website.Html.Edit (editor, serve) where

import Data.Maybe
import Happstack.Server as S

import OpenBrain.Website.Common
import qualified OpenBrain.Website.Html.Decorator as Decorator

data EditorContent = EditorContent {
    editorTitle   :: String
  , ecTitle       :: String
  , ecDescription :: String
  , content       :: String
  , footerLinks   :: [HTML]
  , iid           :: Maybe InformationId
  }
instance Described EditorContent where
  title       = ecTitle
  description = ecDescription
emptyContent = EditorContent {
    editorTitle   = "Create Information"
  , ecTitle       = ""
  , ecDescription = ""
  , content       = ""
  , footerLinks   = [
      "<a id=\"EditorSave\"><img src=\"/files/img/save.svg\" alt=\"Save Information\" title=\"Save Information\"/></a>"
    , "<a id=\"EditorAdd\"><img src=\"/files/img/add.svg\" alt=\"Add as new Information\" title=\"Add as new Information\"/></a>"
    ]
  , iid         = Nothing
  }

{-
  Expected to be one per page for now.
-}
editor :: EditorContent -> OBW HTML
editor eContent = do
  let context "HasIid"                 = MuBool . isJust $ iid eContent
      context "Iid"                    = MuVariable . show . unwrap . toId
                                       . fromJust $ iid eContent
      context "HasTitle"               = MuBool . not . null $ editorTitle eContent
      context "EditorTitle"            = MuVariable $ editorTitle eContent
      context "InformationTitle"       = MuVariable $ title eContent
      context "InformationDescription" = MuVariable $ description eContent
      context "InformationContent"     = MuVariable $ content eContent
      context "FooterLinks"            = MuList . map (mkStrContext . footerContext)
                                       $ footerLinks eContent
  liftIO $ tmpl "MarkdownEditor.html" context
  where
    footerContext h "FooterLink" = htmlToMu h

mkEditor :: EditorContent -> Information -> OBW HTML
mkEditor ec i = case media i of
  (Content s) -> do
    let content = ec{
        ecTitle       = title i
      , ecDescription = description i
      , content       = s
      , iid           = Just $ informationId i
      }
    editor content
  _ -> mzero

{- Serving the editor: -}
serve :: OBW Response
serve = mplus (path lookupInformation) $ do
  p <- Decorator.page =<< editor emptyContent
  ok $ toResponse p

lookupInformation :: Id -> OBW Response
lookupInformation i' = handleFail "Can't find requested Information." $ do
  i <- liftOBB . GetInformation $ fromId i'
  when (isNothing i) mzero
  handleFail "Information is not simple content - can't edit that." $ do
    p <- Decorator.page =<< mkEditor emptyContent{editorTitle = "Edit Information"} (fromJust i)
    ok $ toResponse p
