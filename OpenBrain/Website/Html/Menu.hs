{-# Language OverloadedStrings #-}
module OpenBrain.Website.Html.Menu (menu) where

import Happstack.Server as S

import OpenBrain.Website.Common
import qualified OpenBrain.Website.Html.Login as Login

menu :: OBW HTML
menu = do
  login <- Login.controls
  let context "login" = htmlToMu login
  liftIO $ tmpl "Menu.html" context
    
