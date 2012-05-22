module OpenBrain.Config (
    Config(..)
  , KarmaConfig(..)
  , nullConfig
  , readConfig
  , writeConfig
  , BackendType(..)
) where
{-
  Module for Configuration issues.
-}

import Control.Monad (liftM)
import Data.Maybe (listToMaybe)
import Text.Show.Pretty (ppShow)

import OpenBrain.Config.Karma

type Seconds = Int
data Config = Config {
    fileStorage     :: FilePath -- | Directory to place uploads in and serve static files from
  , allowUploads    :: Bool     -- | Should users be allowed to upload files?
  , allowBrowsing   :: Bool     -- | Should users be able to browse the static files directory?
  , port            :: Int      -- | The port on which openBrain will listen
  , backendType     :: BackendType
  , useTLS          :: Bool
  , tlsKey          :: FilePath
  , tlsCert         :: FilePath
  , sessionTimeout  :: Seconds  -- | Seconds until Sessions are dropped.
  , karmaConfig     :: KarmaConfig
} deriving (Eq, Read, Show)

nullConfig = Config {
    fileStorage     = ""
  , allowUploads    = False
  , allowBrowsing   = False
  , port            = 8000  -- | Happstack std.
  , backendType     = MissingBackend
  , useTLS          = False
  , tlsKey          = ""
  , tlsCert         = ""
  , sessionTimeout  = 900   -- | 15 minutes
  , karmaConfig     = nullKarmaConfig
}

readConfig :: FilePath -> IO (Maybe Config)
readConfig = liftM (liftM fst . listToMaybe . reads) . readFile

writeConfig :: FilePath -> Config -> IO ()
writeConfig path = writeFile path . ppShow

data BackendType = RamBackend -- | All data is only kept in memory
                 | MissingBackend
                   deriving (Eq, Read, Show)
