{-# LANGUAGE DoAndIfThenElse #-}
module OpenBrain.Backend.SqliteBackend.SessionManagement (load) where
{- SessionManagement for the SqliteBackend. -}

import Control.Monad
import Database.HDBC as H

import OpenBrain.Backend
import OpenBrain.Backend.SqliteBackend.Convertibles
import OpenBrain.Data.Salt (mkSalt, fromSalt)
import OpenBrain.Data.User

load :: (IConnection conn) => conn -> SessionManagement
load conn = SessionManagement {
    startSession  = startSession' conn
  , validate      = validate'     conn
  , perform       = perform'      conn
  , stopSession   = stopSession'  conn
  }

startSession' :: (IConnection conn) => conn -> UserId -> IO ActionKey
startSession' conn userid = do
  stmt <- prepare conn "DELETE FROM ActionKeys WHERE userid = ?"
  execute stmt [toSql userid]
  key <- liftM fromSalt mkSalt
  stmt <- prepare conn "INSERT INTO ActionKeys(key, userid) VALUES (?, ?)"
  execute stmt [toSql key, toSql userid]
  commit conn >> return key

validate' :: (IConnection conn) => conn -> UserId -> ActionKey -> IO Bool
validate' conn userid key = do
  rst <- quickQuery' conn "SELECT COUNT(*) FROM ActionKeys WHERE userid = ? AND key = ?" [toSql userid, toSql key]
  case rst of
    [[snum]] -> return $ (fromSql snum :: Int) == 1
    _ -> return False

perform' :: (IConnection conn) => conn -> UserId -> ActionKey -> IO (Maybe ActionKey)
perform' conn userid key = do
  rst <- quickQuery' conn "SELECT COUNT(*) FROM ActionKeys WHERE userid = ? AND key = ?" [toSql userid, toSql key]
  case rst of
    [[snum]] -> do
      if (fromSql snum :: Int) /= 1
      then return Nothing
      else liftM Just $ startSession' conn userid
    _ -> return Nothing

stopSession' :: (IConnection conn) => conn -> UserId -> ActionKey -> IO ()
stopSession' conn userid key = do
  stmt <- prepare conn "DELETE FROM ActionKeys WHERE userid = ? and key = ?"
  execute stmt [toSql userid, toSql key] >> commit conn

