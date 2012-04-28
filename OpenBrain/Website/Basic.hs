{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Basic (
    template, template'
  , keywords, keywords'
  , dummy
) where
{-
  Basic functions that will help in different parts of the Website.
-}
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

type Title = String
type Meta = [H.Html]
type Body = H.Html
template :: Title -> Meta -> Body -> H.Html
template t m b = do
  H.docType
  H.html $ do
    H.head $ do
      H.title $ H.toHtml t
      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
      sequence_ m
    H.body b

template' :: Title -> Body -> H.Html
template' t b = template t [] b

-- | Keywords for meta tags should be comma seperated
type Keywords = String
keywords :: Keywords -> Meta
keywords k = [H.meta ! A.name (H.toValue ("keywords" :: String)) ! A.content (H.toValue k)]

keywords' :: Meta -> Keywords -> Meta
keywords' ms k = keywords k ++ ms

-- | for test reasons
dummy = template' "openBrain testpage" $ H.p "This is only a test."
