{-# Language OverloadedStrings #-}
module OpenBrain.Website.Html.Edit (editor, serve) where

import Data.Maybe
import Happstack.Server as S
import Text.Hastache
import Text.Hastache.Context

import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import OpenBrain.Website.Template
import qualified OpenBrain.Backend.Monad as OBB
import qualified OpenBrain.Data.Information as I
import qualified OpenBrain.Website.Html.Decorator as Decorator

data EditorContent = EditorContent {
    editorTitle :: String
  , title       :: String
  , description :: String
  , content     :: String
  , footerLinks :: [HTML]
  , iid         :: Maybe InformationId
  }
emptyContent = EditorContent {
    editorTitle = "Create Information"
  , title       = ""
  , description = ""
  , content     = ""
  , footerLinks = [
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

mkEditor :: EditorContent -> I.Information -> OBW HTML
mkEditor ec i = case I.media i of
  (I.Content s) -> do
    let content = ec{
        title       = I.title i
      , description = I.description i
      , content     = s
      , iid         = Just $ I.informationId i
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
  i <- liftOBB . OBB.getInformation $ fromId i'
  handleFail "Information is not simple content - can't edit that." $ do
    p <- Decorator.page =<< mkEditor emptyContent{editorTitle = "Edit Information"} i
    ok $ toResponse p
