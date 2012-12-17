module OpenBrain.Backend.PostgreSQLBackend.Sql.SaltShaker where

import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Salt

getSalt' :: (IConnection conn) => UserId -> conn -> IO Salt
getSalt' uid conn = do
  rst <- quickQuery' conn "SELECT salt FROM \"UserData\" WHERE userid = ?" [toSql $ toId uid]
  case rst of
    [[salt]] -> return $ fromSql salt
    _ -> error "Could not fetch salt in OpenBrain.Backend.PostgreSQLBackend.SaltShaker getSalt'."

