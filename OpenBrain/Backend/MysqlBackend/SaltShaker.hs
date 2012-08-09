module OpenBrain.Backend.MysqlBackend.SaltShaker () where

import Database.HDBC as H

import OpenBrain.Backend
import OpenBrain.Backend.MysqlBackend.Convertibles ()
import OpenBrain.Backend.MysqlBackend.Common
import OpenBrain.Data.User
import OpenBrain.Data.Salt

instance SaltShaker MysqlBackend where
  setId       b = withWConn (conn b) setId'
  getSalt     b = withWConn (conn b) getSalt'
  removeSalt  b = withWConn (conn b) removeSalt'

setId' :: (IConnection conn, UserIdentifier ui) => conn -> Salt -> ui -> IO ()
setId' conn salt uid = do
  stmt <- prepare conn "Update UserData SET salt = ? WHERE userid = ?"
  execute stmt [toSql salt, toSql $ getUserId uid] >> commit conn

getSalt' :: (IConnection conn, UserIdentifier ui) => conn -> ui -> IO Salt
getSalt' conn uid = do
  rst <- quickQuery conn "SELECT salt FROM UserData WHERE userid = ?" [toSql $ getUserId uid]
  case rst of
    [[salt]] -> return $ fromSql salt
    _ -> do
      salt <- mkSalt
      setId' conn salt uid
      return salt

removeSalt' :: (IConnection conn, UserIdentifier ui) => conn -> ui -> IO ()
removeSalt' conn uid = do
  stmt <- prepare conn "Update UserData SET salt = '' WHERE userid = ?"
  execute stmt [toSql $ getUserId uid] >> commit conn
