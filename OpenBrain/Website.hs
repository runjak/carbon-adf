{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}
module OpenBrain.Website (serve) where
{-
  This module ties everything website focussed together.
  Therefore it has to do the routing for Website.Sitemap
  and to know everything to handle these requests.
-}
import Control.Monad
import Happstack.Server hiding (port)
import qualified Happstack.Server as S
import Happstack.Server.SimpleHTTPS (nullTLSConf, simpleHTTPS)
import qualified Happstack.Server.SimpleHTTPS as TLS

import OpenBrain.Backend
import OpenBrain.Config (Config(..))
import qualified OpenBrain.Website.Action as Action (serve)
import qualified OpenBrain.Website.Index as Index (serve)
import qualified OpenBrain.Website.Files as Files (serve)
import OpenBrain.Website.Common
import OpenBrain.Website.Monad
import qualified OpenBrain.Website.User as User (serve)
import qualified OpenBrain.Website.Html.User  as HUser
import qualified OpenBrain.Website.Html.Users as HUsers

serve :: CBackend -> Config -> IO ()
serve backend config = do
  let serverParts = runOBW (WebsiteState backend config) serve'
  if useTLS config
  then flip simpleHTTPS serverParts nullTLSConf{
      TLS.tlsPort = port config
    , TLS.tlsKey = tlsKey config
    , TLS.tlsCert = tlsCert config
    }
  else simpleHTTP nullConf{S.port = port config} serverParts

serve' :: OBW Response
serve' = msum [
      dir "action" $ Action.serve
    , dir "files" $ Files.serve
    , contentNego "user"
    , dir "user.html" $ User.serve
    , dir "user" $ path (\username -> contentNego username) -- FIXME this appears to be problematic
    , dir "user" $ HUser.showUser
    , contentNego "users"
    , dir "users.html" $ HUsers.serve
    , Index.serve
    ]
