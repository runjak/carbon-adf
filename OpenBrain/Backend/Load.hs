module OpenBrain.Backend.Load (loadBackend) where
{-
  Backend.Load provides the loading procedure to build a Backend from the Config.
-}
import OpenBrain.Backend
import qualified OpenBrain.Backend.SqliteBackend as Sqlite
import OpenBrain.Config

import Control.Monad

loadBackend :: Config -> IO (Maybe Backend)
loadBackend c = case backendType c of
  (Sqlite3Backend dblocation) -> liftM Just $ Sqlite.load dblocation c
  MissingBackend              -> return Nothing
  
