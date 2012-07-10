{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Images (
    add, clear, edit
  , logout, remove, save
  , users
) where
{-
  Defines easy to replace image locations and syntax to embed them.
-}
import Text.Blaze ((!))
import Text.Blaze.Html (ToMarkup(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

path :: String -> H.AttributeValue
path = H.toValue . ("files/img/" ++)

image :: String -> H.Html
image i = H.img ! A.src (path i)

add     = image "add.svg"
clear   = image "clear.svg"
edit    = image "edit.svg"
logout  = image "logout.svg"
remove  = image "remove.svg"
save    = image "save.svg"
users   = image "users.svg"
