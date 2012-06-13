module OpenBrain.Backend.SqliteBackend.SaltShaker (load) where
{- Provides the SaltShaker for the SqliteBackend. -}

import Database.HDBC as H

import OpenBrain.Backend
import OpenBrain.Backend.SqliteBackend.Convertibles ()
import OpenBrain.Data.User
import OpenBrain.Data.Salt

load :: (IConnection conn) => conn -> SaltShaker
load conn = SaltShaker {
    setId       = setId' conn
  , getSalt     = getSalt' conn
  , removeSalt  = removeSalt' conn
  }

setId' :: (IConnection conn) => conn -> Salt -> UserId -> IO ()
setId' conn salt id = do
  stmt <- prepare conn "INSERT INTO Salts(salt, userid) VALUES (?, ?)"
  execute stmt [toSql salt, toSql id] >> commit conn

getSalt' :: (IConnection conn) => conn -> UserId -> IO Salt
getSalt' conn id = do
  rst <- quickQuery conn "SELECT salt FROM Salts WHERE userid = ?" [toSql id]
  case rst of
    [[salt]] -> return $ fromSql salt
    _ -> do
      salt <- mkSalt
      setId' conn salt id
      return salt

removeSalt' :: (IConnection conn) => conn -> UserId -> IO ()
removeSalt' conn id = do
  stmt <- prepare conn "DELETE FROM Salts WHERE salt = ?"
  execute stmt [toSql id] >> commit conn

