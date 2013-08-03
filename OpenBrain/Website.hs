{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website (serve) where
{-
  This module ties everything website focussed together.
  Therefore it has to do the routing for Website.Sitemap
  and to know everything to handle these requests.
-}
import Happstack.Server hiding (port)
import qualified Happstack.Server as S

import OpenBrain.Website.Common
import OpenBrain.Website.Routes

serve :: CBackendProcessor -> Config -> IO ()
serve backend config =
  let serverParts = runOBW (WebsiteState backend config) route
  in simpleHTTP nullConf{S.port = port config} serverParts
