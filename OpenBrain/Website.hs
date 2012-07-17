{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website (serve) where
{-
  This module ties everything website focussed together.
  Therefore it has to do the routing for Website.Sitemap
  and to know everything to handle these requests.
-}
import Happstack.Server hiding (port)
import Happstack.Server.SimpleHTTPS (nullTLSConf, simpleHTTPS)
import qualified Happstack.Server as S
import qualified Happstack.Server.SimpleHTTPS as TLS

import OpenBrain.Backend
import OpenBrain.Config (Config(..))
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import qualified OpenBrain.Website.Action as Action (serve)
import qualified OpenBrain.Website.Files as Files (serve)
import qualified OpenBrain.Website.Html.Index as HIndex (serve)
import qualified OpenBrain.Website.Html.User as HUser
import qualified OpenBrain.Website.Html.UserControl as HUserControl
import qualified OpenBrain.Website.Html.Users as HUsers

serve :: CBackend -> Config -> IO ()
serve backend config = do
  let serverParts = runOBW (WebsiteState backend config) serve'
  if useTLS config then
    simpleHTTPS nullTLSConf{
        TLS.tlsPort = port config
      , TLS.tlsKey = tlsKey config
      , TLS.tlsCert = tlsCert config
      } serverParts
    else simpleHTTP nullConf{S.port = port config} serverParts

serve' :: OBW Response
serve' = msum [
      dir "action" Action.serve
    , dir "files" Files.serve
    , dir "user" HUser.showUser
    , dir "user" $ path contentNego
    , contentNego' "user"
    , dir "user.html" HUserControl.serve
    , contentNego' "users"
    , dir "users.html" HUsers.serve
    , HIndex.serve
    ]

