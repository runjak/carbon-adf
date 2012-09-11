{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Decorator (Decorator, head, page) where

import Control.Monad.State
import Prelude hiding (head)
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Config
import OpenBrain.Website.Monad
import qualified OpenBrain.Config.Website as W
import qualified OpenBrain.Website.Html.Menu as Menu

type Decorator = H.Html -> OBW H.Html

page :: Decorator
page content = do
  m <- Menu.menu
  head . H.body $ m >> content

{-
  Treats the target as body and adds head, html and doctype.
-}
head :: Decorator
head target = do
  m     <- meta
  js    <- jsFiles
  css     <- cssFiles
  title <- liftM (W.title . websiteConfig) $ gets config
  return $ H.docTypeHtml $ do
    H.head $ do
      H.title $ H.toHtml title
      m >> css >> js
    target

meta :: OBW H.Html
meta = do
  c <- gets config
  return $ do
    H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
    mapM_ go $ W.metaValues $ websiteConfig c
  where
    go (name, content) = H.meta ! A.name (H.toValue name) ! A.content (H.toValue content)

jsFiles :: OBW H.Html
jsFiles = do
  c <- gets config
  return $ mapM_ go $ W.jsFiles $ websiteConfig c
  where
    go target = H.script ! A.type_ "application/javascript" ! A.src (H.toValue target) $ ""

cssFiles :: OBW H.Html
cssFiles = do
  c <- gets config
  return $ forM_ (W.cssFiles $ websiteConfig c) $ \file -> H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (H.toValue file)

