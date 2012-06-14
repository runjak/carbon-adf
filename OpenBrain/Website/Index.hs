{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Index (serve) where
{-
  Provides the index page.
-}

import Control.Monad
import Control.Monad.Trans(liftIO)
import Happstack.Server as S
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import OpenBrain.Backend
import OpenBrain.Config
import qualified OpenBrain.Config.Website as W

serve :: Backend -> Config -> ServerPartT IO Response
serve backend config = do
  ok . toResponse . H.docTypeHtml $ do
    H.head $ do
      H.title . H.toHtml . W.title $ websiteConfig config
      sequence_ $ meta config
      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
      sequence_ $ jsfiles config
    H.body $ do
      H.p "This is how we do it."

meta :: Config -> [H.Html]
meta config = map go . W.metaValues $ websiteConfig config
  where
    go (name, content) = H.meta ! A.name (H.toValue name) ! A.content (H.toValue content)

jsfiles :: Config -> [H.Html]
jsfiles config = map go . W.jsFiles $ websiteConfig config
  where
    go target = H.script ! A.type_ "application/javascript" ! A.src (H.toValue target) $ ""
