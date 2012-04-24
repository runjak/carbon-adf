module Website.Files (serve) where
{-
  This module serves the static files on the website
  which are located in /files/.
  It uses the Config.File module for configuration.
-}
import Config (Config(..))
import Happstack.Server (Browsing(..), serveDirectory, dir)
import qualified Happstack.Server as S

serve conf = do
  let b = browsing $ allowBrowsing conf
  dir "files" . serveDirectory b fallbackFiles $ fileStorage conf

browsing :: Bool -> Browsing
browsing True   = EnableBrowsing
browsing False  = DisableBrowsing

{-
  serve will try to serve these if the requested file is not found.
-}
fallbackFiles :: [String]
fallbackFiles = ["index.html"]
