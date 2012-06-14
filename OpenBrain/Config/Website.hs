module OpenBrain.Config.Website (
    WebsiteConfig(..)
  , nullWebsiteConfig
) where
{- Configuration of static Website values. -}

data WebsiteConfig = WebsiteConfig {
    title       :: String
  , metaValues  :: [(String, String)]
  , jsFiles     :: [String]
  } deriving (Eq, Show, Read)

nullWebsiteConfig = WebsiteConfig {
    title       = "OpenBrain v0.1 - demo of a dream"
  , metaValues  = [("generator", "OpenBrain v0.1"),("author", "Hλskell-lover J.Runge")]  
  , jsFiles     = map ("files/js/"++) [
      "jquery.js"
    , "jquery.cookie.js"
    , "sha512.js"
    , "test.js"]
  }
