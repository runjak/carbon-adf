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

import OpenBrain.Config.Karma

type Seconds = Int
data Config = Config {
    fileStorage     :: FilePath -- | Directory to place uploads in and serve static files from
  , allowUploads    :: Bool     -- | Should users be allowed to upload files?
  , allowBrowsing   :: Bool     -- | Should users be able to browse the static files directory?
  , port            :: Int      -- | The port on which openBrain will listen
  , backendType     :: BackendType
  , tlsKey          :: FilePath
  , tlsCert         :: FilePath
  , karmaConfig     :: KarmaConfig
} deriving (Eq, Read, Show)

nullConfig = Config {
    fileStorage     = "files/"
  , allowUploads    = False
  , allowBrowsing   = True
  , port            = 8000  -- | Happstack std.
  , backendType     = PostgreSQLBackend {pgOptions = "dbname=openbrain host=127.0.0.1 user=mushu password=1234"}
  , tlsKey          = ""
  , tlsCert         = ""
  , karmaConfig     = nullKarmaConfig
}

readConfig :: FilePath -> IO (Maybe Config)
readConfig = liftM (liftM fst . listToMaybe . reads) . readFile

writeConfig :: FilePath -> Config -> IO ()
writeConfig path = writeFile path . show

data BackendType = MissingBackend
                 | PostgreSQLBackend {
                   pgOptions :: String -- As described in http://www.postgresql.org/docs/8.1/static/libpq.html#LIBPQ-CONNECT
                 }
                 deriving (Eq, Read, Show)
