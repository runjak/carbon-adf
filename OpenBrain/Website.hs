{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website (serve) where
{-
  This module ties everything website focussed together.
  Therefore it has to do the routing for Website.Sitemap
  and to know everything to handle these requests.
-}
import Happstack.Server hiding (port)

import qualified Happstack.Server as S

import OpenBrain.Backend
import OpenBrain.Config (Config(..))
import OpenBrain.Website.Common
import OpenBrain.Website.Monad

import qualified OpenBrain.Website.Action           as Action (serve)
import qualified OpenBrain.Website.Files            as Files (serve)
import qualified OpenBrain.Website.Html.Edit        as Edit (serve)
import qualified OpenBrain.Website.Html.Index       as Index (serve)
import qualified OpenBrain.Website.Html.Information as Information (serve)
import qualified OpenBrain.Website.Html.User        as User(serve)

serve :: Backend -> Config -> IO ()
serve backend config =
  let serverParts = runOBW (WebsiteState backend config) serve'
  in simpleHTTP nullConf{S.port = port config} serverParts

serve' :: OBW Response
serve' = msum [
      dir "action" Action.serve
    , dir "edit" Edit.serve
    , dir "files" Files.serve
    , dir "information.html" Information.serve
    , contentNego' "information"
    , dir "user.html" User.serve
    , contentNego' "user"
    , Index.serve
    ]

