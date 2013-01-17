{-# LANGUAGE ScopedTypeVariables #-}
module OpenBrain.Backend.PostgreSQLBackend.Sql.SessionManagement where

import System.Random

import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Data

startSession' :: (IConnection conn) => UserId -> conn -> IO ActionKey
startSession' uid conn = do
  let userid = toId uid
  stmt <- prepare conn "UPDATE \"UserData\" SET actionkey = NULL WHERE userid = ?"
  execute stmt [toSql userid]
  (r:rs) <- liftM randoms newStdGen
  let l = 10 + (r `mod` 11)
  let (key :: String) = map (toEnum . (+ fromEnum 'a') . flip mod (fromEnum '~' - fromEnum 'a')) $ take l rs
  stmt <- prepare conn "UPDATE \"UserData\" SET actionkey = ? WHERE userid = ?"
  execute stmt [toSql key, toSql userid]
  return key

validate' :: (IConnection conn) => UserId -> ActionKey -> conn -> IO Bool
validate' uid key conn = do
  rst <- quickQuery' conn "SELECT COUNT(*) FROM \"UserData\" WHERE userid = ? AND actionkey = ?" [toSql $ toId uid, toSql key]
  case rst of
    [[snum]] -> return $ (fromSql snum :: Int) == 1
    _ -> return False

stopSession' :: (IConnection conn) => UserId -> ActionKey -> conn -> IO ()
stopSession' uid key conn = do
  stmt <- prepare conn "UPDATE \"UserData\" SET actionkey = NULL WHERE userid = ? AND actionkey = ?"
  void $ execute stmt [toSql $ toId uid, toSql key]

