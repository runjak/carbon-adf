{-# Language OverloadedStrings #-}
module OpenBrain.Website.Html.Menu (menu) where

import Happstack.Server as S
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Website.Monad
import qualified OpenBrain.Website.Html.Images as Images
import qualified OpenBrain.Website.Html.Login as Login

menu :: OBW H.Html
menu = do
  login <- Login.controls
  return $ H.ul ! A.id "menu" $ do
    H.li ! A.class_ "icon" ! A.id "MenuLogo" $ do
      H.a ! A.href "/" $ Images.openBrain' "OpenBrain" "OpenBrain"
    H.li ! A.class_ "icon" ! A.id "MenuLogin" $ do
      Images.users' "LogIn/Out" "LogIn/Out"
      login
    H.li ! A.class_ "icon" ! A.id "MenuEditNewInformation" $ do
      H.a ! A.href "/edit" $ Images.edit' "Create new Information" "Create new Information"

