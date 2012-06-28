{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Decorator (Decorator, head) where

import Control.Monad.State
import Prelude hiding (head)
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Config
import OpenBrain.Website.Monad
import qualified OpenBrain.Config.Website as W

type Decorator = OBW H.Html -> OBW H.Html

{-
  Treats the target as body and adds head, html and doctype.
-}
head :: Decorator
head target = do
  m     <- meta
  js    <- jsfiles
  body  <- target
  title <- liftM (W.title . websiteConfig) $ gets config
  return $ H.docTypeHtml $ do
    H.head $ do
      H.title $ H.toHtml title
      m >> js
    H.body body

meta :: OBW H.Html
meta = do
  c <- gets config
  return $ do
    H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
    mapM_ go $ W.metaValues $ websiteConfig c
  where
    go (name, content) = H.meta ! A.name (H.toValue name) ! A.content (H.toValue content)

jsfiles :: OBW H.Html
jsfiles = do
  c <- gets config
  return $ mapM_ go $ W.jsFiles $ websiteConfig c
  where
    go target = H.script ! A.type_ "application/javascript" ! A.src (H.toValue target) $ ""

