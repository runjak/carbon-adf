{-# LANGUAGE OverloadedStrings #-}
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
import qualified Happstack.Server as S

serve :: (Backend b) => b -> Config -> IO ()
serve backend config = do
  simpleHTTP nullConf{S.port = port config} $ msum
    [ dir "files" $ F.serve config
    , S.ok $ S.toResponse dummy
    ]
