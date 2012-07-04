{-# LANGUAGE DoAndIfThenElse, ScopedTypeVariables #-}
module OpenBrain.Backend.SqliteBackend.SessionManagement () where
{- SessionManagement for the SqliteBackend. -}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Database.HDBC as H
import System.Random

import OpenBrain.Backend
import OpenBrain.Backend.SqliteBackend.Convertibles
import OpenBrain.Backend.SqliteBackend.Common
import OpenBrain.Data.User

instance SessionManagement SqliteBackend where
  startSession b = withWConn (conn b) startSession'
  validate     b = withWConn (conn b) validate'
  perform      b = withWConn (conn b) perform'
  stopSession  b = withWConn (conn b) stopSession'

startSession' :: (IConnection conn) => conn -> UserId -> IO ActionKey
startSession' conn userid = do
  stmt <- prepare conn "DELETE FROM ActionKeys WHERE userid = ?"
  execute stmt [toSql userid]
  (r:rs) <- liftM randoms newStdGen
  let l = 10 + (r `mod` 11)
  let (key :: String) = map (toEnum . (+ fromEnum 'a') . flip mod (fromEnum '~' - fromEnum 'a')) $ take l rs
  stmt <- prepare conn "INSERT INTO ActionKeys(key, userid) VALUES (?, ?)"
  execute stmt [toSql key, toSql userid]
  commit conn >> return key

validate' :: (IConnection conn) => conn -> UserId -> ActionKey -> IO Bool
validate' conn userid key = do
  rst <- quickQuery' conn "SELECT COUNT(*) FROM ActionKeys WHERE userid = ? AND key = ?" [toSql userid, toSql key]
  case rst of
    [[snum]] -> return $ (fromSql snum :: Int) == 1
    _ -> return False

perform' :: (IConnection conn) => conn -> UserId -> ActionKey -> MaybeT IO ActionKey
perform' conn userid key = do
  rst <- liftIO $ quickQuery' conn "SELECT COUNT(*) FROM ActionKeys WHERE userid = ? AND key = ?" [toSql userid, toSql key]
  case rst of
    [[snum]] -> do
      guard $ (fromSql snum :: Int) == 1
      liftIO $ startSession' conn userid
    _ -> mzero

stopSession' :: (IConnection conn) => conn -> UserId -> ActionKey -> IO ()
stopSession' conn userid key = do
  stmt <- prepare conn "DELETE FROM ActionKeys WHERE userid = ? and key = ?"
  execute stmt [toSql userid, toSql key] >> commit conn

