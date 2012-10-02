{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Html.Images where
{-
  Defines easy to replace image locations and syntax to embed them.
-}
import Text.Blaze ((!))
import Text.Blaze.Html (ToMarkup(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

path :: String -> H.AttributeValue
path = H.toValue . ("/files/img/" ++)

image :: String -> H.Html
image i = H.img ! A.src (path i)

type Alt    = String
type Title  = String
imageLabel :: String -> Alt -> Title -> H.Html
imageLabel i a' t' = let a = H.toValue a'
                         t = H.toValue t'
                     in H.img ! A.alt a ! A.title t ! A.src (path i)

add         = image "add.svg"
add'        = imageLabel "add.svg"
book        = image "book.svg"
book'       = imageLabel "book.svg"
bookmark    = image "bookmark.svg"
bookmark'   = imageLabel "bookmark.svg"
clear       = image "clear.svg"
clear'      = imageLabel "clear.svg"
edit        = image "edit.svg"
edit'       = imageLabel "edit.svg"
favorite    = image "favorite.svg"
favorite'   = imageLabel "favorite.svg"
logout      = image "logout.svg"
logout'     = imageLabel "logout.svg"
openBrain   = image "OpenBrain.svg"
openBrain'  = imageLabel "OpenBrain.svg"
remove      = image "remove.svg"
remove'     = imageLabel "remove.svg"
save        = image "save.svg"
save'       = imageLabel "save.svg"
stop        = image "stop.svg"
stop'       = imageLabel "stop.svg"
users       = image "users.svg"
users'  = imageLabel "users.svg"

