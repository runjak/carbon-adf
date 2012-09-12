{-# Language OverloadedStrings #-}
module OpenBrain.Website.Html.Menu (menu) where

import Happstack.Server as S
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Website.Monad

menu :: OBW H.Html
menu = return menu'

menu' :: H.Html
menu' = H.ul ! A.id "menu" $ do
  mapM_ H.li ["Some", "Dummy", "Entries"]

