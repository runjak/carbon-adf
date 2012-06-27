module OpenBrain.Backend.Load (loadBackend) where
{-
  Backend.Load provides the loading procedure to build a Backend from the Config.
-}
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import OpenBrain.Backend
import qualified OpenBrain.Backend.SqliteBackend as Sqlite
import OpenBrain.Config
import OpenBrain.Common

loadBackend :: Config -> MaybeT IO CBackend
loadBackend c = case backendType c of
  (Sqlite3Backend dblocation) -> liftIOM CBackend $ Sqlite.load dblocation c
  MissingBackend              -> mzero
  
