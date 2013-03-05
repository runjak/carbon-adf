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
import qualified OpenBrain.Deadline                 as Deadline
import qualified OpenBrain.Website.Action           as Action (serve)
import qualified OpenBrain.Website.Files            as Files (serve)
import qualified OpenBrain.Website.Html.Information as Information (serve)
import qualified OpenBrain.Website.Html.User        as User(serve)

serve :: CBackend -> Config -> IO ()
serve backend config = do
  deadline <- Deadline.newState
  let serverParts = runOBW (WebsiteState backend config deadline) serve'
  simpleHTTP nullConf{S.port = port config} serverParts

serve' :: OBW Response
serve' = msum [
      dir "action" Action.serve
    , dir "files" Files.serve
    , dir "information.html" Information.serve
    , contentNego' "information"
    , dir "user.html" User.serve
    , contentNego' "user"
    , liftM responseHTML $ S.serveFile return "files/index.html"
    ]

