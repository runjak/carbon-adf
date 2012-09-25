{-# LANGUAGE ScopedTypeVariables #-}
module OpenBrain.Backend.PostgreSQLBackend.SessionManagement () where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Database.HDBC as H
import System.Random

import OpenBrain.Backend
import OpenBrain.Backend.PostgreSQLBackend.Convertibles
import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.Types
import OpenBrain.Data.Id

instance SessionManagement PostgreSQLBackend where
  startSession b = withWConn (conn b) startSession'
  validate     b = withWConn (conn b) validate'
  stopSession  b = withWConn (conn b) stopSession'

startSession' :: (IConnection conn) => conn -> UserId -> IO ActionKey
startSession' conn uid = do
  let userid = toId uid
  stmt <- prepare conn "UPDATE \"UserData\" SET actionkey = NULL WHERE userid = ?"
  execute stmt [toSql userid]
  (r:rs) <- liftM randoms newStdGen
  let l = 10 + (r `mod` 11)
  let (key :: String) = map (toEnum . (+ fromEnum 'a') . flip mod (fromEnum '~' - fromEnum 'a')) $ take l rs
  stmt <- prepare conn "UPDATE \"UserData\" SET actionkey = ? WHERE userid = ?"
  execute stmt [toSql key, toSql userid]
  commit conn >> return key

validate' :: (IConnection conn) => conn -> UserId -> ActionKey -> IO Bool
validate' conn uid key = do
  rst <- quickQuery' conn "SELECT COUNT(*) FROM \"UserData\" WHERE userid = ? AND actionkey = ?" [toSql $ toId uid, toSql key]
  case rst of
    [[snum]] -> return $ (fromSql snum :: Int) == 1
    _ -> return False

stopSession' :: (IConnection conn) => conn -> UserId -> ActionKey -> IO ()
stopSession' conn uid key = do
  stmt <- prepare conn "UPDATE \"UserData\" SET actionkey = NULL WHERE userid = ? AND actionkey = ?"
  execute stmt [toSql $ toId uid, toSql key] >> commit conn

