module OpenBrain.Backend.SqliteBackend.Common where
{- Definition of the SqliteBackend, that will be enriched with instances to become a full Backend. -}

import Database.HDBC as H

import OpenBrain.Config

data SqliteBackend = SqliteBackend {
    config  :: Config
  , conn    :: ConnWrapper
}

lastInsertRowId :: (IConnection conn) => conn -> IO Integer
lastInsertRowId conn = do
  rst <- quickQuery conn "SELECT last_insert_rowid()" []
  case rst of
    [[sRowId]] -> return $ fromSql sRowId
    _ -> error "Could not select last_insert_rowid in OpenBrain.Backend.SqliteBackend.Common.lastInsertRowId"

