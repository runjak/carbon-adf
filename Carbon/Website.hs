{-# LANGUAGE OverloadedStrings #-}
module Carbon.Website (serve) where
{-
  This module ties everything website focussed together.
  Therefore it has to do the routing for Website.Sitemap
  and to know everything to handle these requests.
-}
import Happstack.Server hiding (port)
import qualified Happstack.Server as S

import Carbon.Website.Common
import Carbon.Website.Routes (route)
import qualified Carbon.Backend.User as BUser
import qualified Carbon.Website.Composition as Composition

serve :: CBackendProcessor -> Config -> IO ()
serve backend config = do
  (t, _) <- Composition.compose $ composition config
  let state = WebsiteState backend config t
      parts = runOBW state route
  process backend BUser.init
  simpleHTTP nullConf{S.port = port config} parts
