module OpenBrain.Backend.MysqlBackend.SaltShaker () where

import Database.HDBC as H

import OpenBrain.Backend
import OpenBrain.Backend.MysqlBackend.Convertibles ()
import OpenBrain.Backend.MysqlBackend.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Salt

instance SaltShaker MysqlBackend where
  setId       b = withWConn (conn b) setId'
  getSalt     b = withWConn (conn b) getSalt'
  removeSalt  b = withWConn (conn b) removeSalt'

setId' :: (IConnection conn) => conn -> Salt -> UserId -> IO ()
setId' conn salt uid = do
  stmt <- prepare conn "Update UserData SET salt = ? WHERE userid = ?"
  execute stmt [toSql salt, toSql $ toId uid] >> commit conn

getSalt' :: (IConnection conn) => conn -> UserId -> IO Salt
getSalt' conn uid = do
  rst <- quickQuery' conn "SELECT salt FROM UserData WHERE userid = ?" [toSql $ toId uid]
  case rst of
    [[salt]] -> return $ fromSql salt
    _ -> do
      salt <- mkSalt
      setId' conn salt uid
      return salt

removeSalt' :: (IConnection conn) => conn -> UserId -> IO ()
removeSalt' conn uid = do
  stmt <- prepare conn "Update UserData SET salt = '' WHERE userid = ?"
  execute stmt [toSql $ toId uid] >> commit conn
