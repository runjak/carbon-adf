module OpenBrain.Website.Files (serve) where
{-
  This module serves the static files on the website
  which are located in /files/.
  It uses the Config.File module for configuration.
-}
import Control.Monad.State
import Happstack.Server (Browsing(..), serveDirectory, Response)
import qualified Happstack.Server as S

import OpenBrain.Config (Config(..))
import OpenBrain.Website.Monad

serve :: OBW Response
serve = do
  conf <- gets config
  let b = browsing $ allowBrowsing conf
  serveDirectory b fallbackFiles $ fileStorage conf

browsing :: Bool -> Browsing
browsing True   = EnableBrowsing
browsing False  = DisableBrowsing

{-
  serve will try to serve these if the requested file is not found.
-}
fallbackFiles :: [String]
fallbackFiles = ["index.html"]
