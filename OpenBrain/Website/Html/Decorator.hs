{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Decorator (Decorator, head, page) where

import Control.Monad.State
import Prelude hiding (head)

import OpenBrain.Website.Common
import qualified OpenBrain.Config.Website as W
import qualified OpenBrain.Website.Html.Menu as Menu

type Decorator = HTML -> OBW HTML

page :: Decorator
page c = do
  h <- head
  m <- Menu.menu
  let context "head"    = htmlToMu h
      context "menu"    = htmlToMu m
      context "content" = htmlToMu c
  liftIO $ tmpl "Page.html" context

head :: OBW HTML
head = do
  c <- gets (websiteConfig . config)
  let context "title"     = MuVariable $ W.title c
      context "metaInfos" = MuList . map (mkStrContext . metaInfoContext) $ W.metaValues c
      context "cssFiles"  = MuList . map (mkStrContext . cssFileContext)  $ W.cssFiles c
      context "jsFiles"   = MuList . map (mkStrContext . jsFileContext)   $ W.jsFiles c
  liftIO $ tmpl "Head.html" context
  where
    metaInfoContext (metaName, metaContent) "metaName"    = MuVariable metaName
    metaInfoContext (metaName, metaContent) "metaContent" = MuVariable metaContent
    cssFileContext cssFile "cssFile" = MuVariable cssFile
    jsFileContext jsFile   "jsFile"  = MuVariable jsFile

