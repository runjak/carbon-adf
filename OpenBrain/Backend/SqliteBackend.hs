module OpenBrain.Backend.SqliteBackend (load) where

import Database.HDBC as H
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import qualified Database.HDBC.Sqlite3 as S

import OpenBrain.Backend as B
import OpenBrain.Backend.SqliteBackend.Schema (initTables)
import qualified OpenBrain.Backend.SqliteBackend.KarmaBackend as K (load)
import qualified OpenBrain.Backend.SqliteBackend.SaltShaker   as S (load)
import qualified OpenBrain.Backend.SqliteBackend.UserBackend  as U (load)
import OpenBrain.Config as C

createTables :: IConnection conn => conn -> IO ()
createTables conn = mapM_ (\t -> run conn t []) initTables >> commit conn

load :: FilePath -> Config -> IO Backend
load dblocation config = do
  conn <- connectSqlite3 dblocation  
  createTables conn
  return $ Backend {
      shutdown      = commit conn >> disconnect conn
    , userBackend   = U.load conn
    , karmaBackend  = K.load conn $ karmaConfig config
    , saltShaker    = S.load conn
    }
