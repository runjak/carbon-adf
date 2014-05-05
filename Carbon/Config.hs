module Carbon.Config (
    Config(..)
  , nullConfig
  , readConfig
  , writeConfig
  , BackendType(..)
) where
{-
  Module for Configuration issues.
-}

import Control.Monad (liftM)
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map

import Carbon.Config.Composition
import Carbon.Data.Logic.Diamond (ResultType(..))

data Config = Config {
    fileStorage   :: FilePath    -- | Directory to serve static files from
  , allowBrowsing :: Bool        -- | Should users be able to browse the static files directory?
  , port          :: Int         -- | The port on which openBrain will listen
  , backendType   :: BackendType -- | Which backend will be used
  , diamondCall   :: String      -- | What to execute to run diamond.py
  , diamondDlDir  :: FilePath    -- | Which directory to place generated .dl files in
  , diamondParams :: Map ResultType [String] -- | Parameters to pass diamond for each ResultType
  , diamondEval   :: [ResultType] -- | ResultTypes that will be evaluated
  , composition   :: Composition
} deriving (Eq, Read, Show)

nullConfig = Config {
    fileStorage   = "files/"
  , allowBrowsing = True
  , port          = 8000  -- | Happstack std.
--, backendType   = PostgreSQLBackend {pgOptions = "dbname=carbon host=127.0.0.1 user=mushu"}
  , backendType   = PostgreSQLBackend {pgOptions = "dbname=carbon host=127.0.0.1 user=mushu password=1234"}
  , diamondCall   = "python3"
  , diamondDlDir  = "/tmp/"
  , diamondParams = Map.fromList [(TwoValued,  ["../extern/diamond-adf-code/diamond.py","-pf","-v0","--model"])
                                 ,(Stable,     ["../extern/diamond-adf-code/diamond.py","-pf","-v0","--stablemodel"])
                                 ,(Grounded,   ["../extern/diamond-adf-code/diamond.py","-pf","-v0","--grounded"])
                                 ,(Complete,   ["../extern/diamond-adf-code/diamond.py","-pf","-v0","--complete"])
                                 ,(Admissible, ["../extern/diamond-adf-code/diamond.py","-pf","-v0","--admissible"])
                                 ,(Preferred,  ["../extern/diamond-adf-code/diamond.py","-pf","-v0","--preferred"])]
  , diamondEval   = [TwoValued, Stable, Grounded, Complete, Admissible, Preferred]
  , composition   = defaultComposition
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
