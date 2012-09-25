module OpenBrain.Backend.PostgreSQLBackend.SaltShaker () where

import Database.HDBC as H

import OpenBrain.Backend
import OpenBrain.Backend.PostgreSQLBackend.Convertibles ()
import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Salt

instance SaltShaker PostgreSQLBackend where
  getSalt     b = withWConn (conn b) getSalt'

getSalt' :: (IConnection conn) => conn -> UserId -> IO Salt
getSalt' conn uid = do
  rst <- quickQuery' conn "SELECT salt FROM \"UserData\" WHERE userid = ?" [toSql $ toId uid]
  case rst of
    [[salt]] -> return $ fromSql salt
    _ -> error "Could not fetch salt in OpenBrain.Backend.PostgreSQLBackend.SaltShaker getSalt'."

