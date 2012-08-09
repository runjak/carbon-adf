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
import OpenBrain.Config.Website

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
  , karmaConfig     :: KarmaConfig
  , websiteConfig   :: WebsiteConfig
} deriving (Eq, Read, Show)

nullConfig = Config {
    fileStorage     = "files/"
  , allowUploads    = False
  , allowBrowsing   = True
  , port            = 8000  -- | Happstack std.
  , backendType     = MysqlBackend {mysqlHost = "127.0.0.1", mysqlUser = "root", mysqlPassword = "1234", mysqlDatabase = "OpenBrain", mysqlPort = 3306, mysqlSchemaUpdate = Nothing}
  , useTLS          = False
  , tlsKey          = ""
  , tlsCert         = ""
  , karmaConfig     = nullKarmaConfig
  , websiteConfig   = nullWebsiteConfig
}

readConfig :: FilePath -> IO (Maybe Config)
readConfig = liftM (liftM fst . listToMaybe . reads) . readFile

writeConfig :: FilePath -> Config -> IO ()
writeConfig path = writeFile path . show

data BackendType = Sqlite3Backend {dblocation :: FilePath} -- | Default was Sqlite3Backend{dblocation = "/tmp/test.sqlite"}
                 | MissingBackend
                 | MysqlBackend {
                    mysqlHost         :: String
                  , mysqlUser         :: String
                  , mysqlPassword     :: String
                  , mysqlDatabase     :: String
                  , mysqlPort         :: Int -- | Default is 3306
                  , mysqlSchemaUpdate :: Maybe FilePath}
                 deriving (Eq, Read, Show)
