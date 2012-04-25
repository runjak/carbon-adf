{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}
module Website.Main (serve) where
{-
  This module ties everything website focussed together.
  Therefore it has to do the routing for Website.Sitemap
  and to know everything to handle these requests.
-}
import Backend (Backend)
import Config (Config(..))
import qualified Website.Files as F (serve)

import Website.Basic (dummy)

import Control.Monad
import Happstack.Server (nullConf, simpleHTTP, dir)
import Happstack.Server.SimpleHTTPS (nullTLSConf, simpleHTTPS)
import qualified Happstack.Server as S
import qualified Happstack.Server.SimpleHTTPS as TLS

serve :: (Backend b) => b -> Config -> IO ()
serve backend config = do
  if useTLS config
  then flip simpleHTTPS serverParts nullTLSConf{
      TLS.tlsPort = port config
    , TLS.tlsKey = tlsKey config
    , TLS.tlsCert = tlsCert config
    }
  else simpleHTTP nullConf{S.port = port config} serverParts
  where
    serverParts = msum
      [ dir "files" $ F.serve config
      , S.ok $ S.toResponse dummy
      ]
