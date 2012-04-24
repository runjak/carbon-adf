module Config (
    Config(..)
  , nullConfig
  , readConfig
  , writeConfig
) where
{-
  Module for Configuration issues.
-}

import Control.Monad (liftM)
import Data.Maybe (listToMaybe)

data Config = Config {
    fileStorage   :: FilePath -- | Directory to place uploads in and serve static files from
  , allowUploads  :: Bool     -- | Should users be allowed to upload files?
  , allowBrowsing :: Bool     -- | Should users be able to browse the static files directory?
} deriving (Eq, Read, Show)

nullConfig = Config "" False False

readConfig :: FilePath -> IO (Maybe Config)
readConfig = liftM (liftM fst . listToMaybe . reads) . readFile

writeConfig :: FilePath -> Config -> IO ()
writeConfig path = writeFile path . show
