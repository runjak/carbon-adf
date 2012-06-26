module OpenBrain.Backend.SqliteBackend (load) where

import Database.HDBC as H
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import qualified Database.HDBC.Sqlite3 as S

import OpenBrain.Backend as B
import OpenBrain.Backend.SqliteBackend.Common
import OpenBrain.Backend.SqliteBackend.Schema (initTables)
import qualified OpenBrain.Backend.SqliteBackend.KarmaBackend ()
import qualified OpenBrain.Backend.SqliteBackend.SaltShaker ()
import qualified OpenBrain.Backend.SqliteBackend.SessionManagement ()
import qualified OpenBrain.Backend.SqliteBackend.UserBackend ()
import OpenBrain.Config as C

createTables :: IConnection conn => conn -> IO ()
createTables conn = mapM_ (\t -> run conn t []) initTables >> commit conn

instance Backend SqliteBackend where
  shutdown          b = commit (conn b) >> disconnect (conn b)
  userBackend       = CUserBackend
  karmaBackend      = CKarmaBackend
  saltShaker        = CSaltShaker
  sessionManagement = CSessionManagement

load :: FilePath -> Config -> IO CBackend
load dblocation config = do
  conn <- connectSqlite3 dblocation  
  createTables conn
  return . CBackend . SqliteBackend config $ ConnWrapper conn
