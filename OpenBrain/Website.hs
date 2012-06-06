{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}
module OpenBrain.Website (serve) where
{-
  This module ties everything website focussed together.
  Therefore it has to do the routing for Website.Sitemap
  and to know everything to handle these requests.
-}
import OpenBrain.Backend (Backend)
import OpenBrain.Config (Config(..))
import qualified OpenBrain.Website.Action as Action (serve)
import OpenBrain.Website.Session (newSessionManager)
import qualified OpenBrain.Website.Files as Files (serve)

import OpenBrain.Website.Basic (dummy)

import Control.Monad
import Happstack.Server hiding (port)
import qualified Happstack.Server as S
import Happstack.Server.SimpleHTTPS (nullTLSConf, simpleHTTPS)
import qualified Happstack.Server.SimpleHTTPS as TLS

serve :: Backend -> Config -> IO ()
serve backend config = do
  serverParts <- serve' backend config
  if useTLS config
  then flip simpleHTTPS serverParts nullTLSConf{
      TLS.tlsPort = port config
    , TLS.tlsKey = tlsKey config
    , TLS.tlsCert = tlsCert config
    }
  else simpleHTTP nullConf{S.port = port config} serverParts

serve' :: Backend -> Config -> IO (ServerPartT IO Response)
serve' backend config = do
  sessionmanager <- newSessionManager config
  return $ msum [
      dir "action" $ Action.serve backend sessionmanager
    , dir "files" $ Files.serve config
    , ok $ toResponse dummy
    ]