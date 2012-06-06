module OpenBrain.Backend.SqliteBackend.UserBackend (load) where
{- Provides the UserBackend for SqliteBackend. -}

import Control.Monad
import Database.HDBC as H
import System.Time as T

import OpenBrain.Backend
import OpenBrain.Backend.SqliteBackend.Convertibles ()
import qualified OpenBrain.Backend.SqliteBackend.ProfileBackend as P (load)
import OpenBrain.User.Hash
import OpenBrain.User.Profile
import OpenBrain.User.Data

load :: (IConnection conn) => conn -> UserBackend
load conn = UserBackend {
    login           = login'            conn
  , getUser         = getUser'          conn
  , hasUserWithId   = hasUserWithId'    conn
  , hasUserWithName = hasUserWithName'  conn
  , register        = register'         conn
  , delete          = delete'           conn
  , profileBackend  = P.load conn
  }

login' :: (IConnection conn) => conn -> UserName -> Hash -> IO (Maybe UserData)
login' conn username hash = do
  rst <- quickQuery conn "SELECT * FROM UserData WHERE username = ? AND password = ?" [toSql username, toSql hash]
  case rst of
    [[userid', _, _, karma', creation', lastLogin', isAdmin']] -> do
      let userdata = UserData {
          userid    = fromSql userid'
        , username  = username
        , password  = hash
        , karma     = fromSql karma'
        , creation  = fromSql creation'
        , lastLogin = fromSql lastLogin'
        , isAdmin   = fromSql isAdmin'
        }
      t <- liftM toUTCTime getClockTime
      stmt <- prepare conn "UPDATE UserData SET lastLogin = ? WHERE userid = ?"
      execute stmt [toSql t, toSql (userid userdata)] >> commit conn
      return $ Just userdata{lastLogin = t}
    _ -> return Nothing

getUser' :: (IConnection conn) => conn -> UserId -> IO (Maybe UserData)
getUser' conn userid = do
  rst <- quickQuery conn "SELECT * FROM UserData WHERE userid = ?" [toSql userid]
  case rst of
    [[_, username', password', karma', creation', lastLogin', isAdmin']] -> return $ Just UserData {
        userid    = userid
      , username  = fromSql username'
      , password  = fromSql password'
      , karma     = fromSql karma'
      , creation  = fromSql creation'
      , lastLogin = fromSql lastLogin'
      , isAdmin   = fromSql isAdmin'
      }
    _ -> return Nothing

hasUserWithId' :: (IConnection conn) => conn -> UserId -> IO Bool
hasUserWithId' conn userid = do
  rst <- quickQuery conn "SELECT COUNT(*) FROM UserData WHERE userid = ?" [toSql userid]
  case rst of
    [[count]] -> return $ (>0) (fromSql count :: Int)
    _         -> return False

hasUserWithName' :: (IConnection conn) => conn -> UserName -> IO Bool
hasUserWithName' conn username = do
  rst <- quickQuery conn "SELECT COUNT(*) FROM UserData WHERE username = ?" [toSql username]
  case rst of
    [[count]] -> return $ (>0) (fromSql count :: Int)
    _         -> return False

register' :: (IConnection conn) => conn -> UserName -> Hash -> IO (Maybe UserData)
register' conn username hash = do
  duplicate <- hasUserWithName' conn username
  case duplicate of
    True  -> return Nothing
    False -> do
      t <- liftM (toSql . toUTCTime) getClockTime
      stmt <- prepare conn "INSERT INTO UserData(username, password, creation, lastLogin) VALUES (?, ?, ?, ?)"
      execute stmt [toSql username, toSql hash, t ,t] >> commit conn
      login' conn username hash

delete' :: (IConnection conn) => conn -> UserId -> IO Bool
delete' conn userid = do
  stmt  <- prepare conn "DELETE FROM UserData WHERE userid = ?"
  rst   <- execute stmt [toSql userid]
  commit conn
  return $ rst > 0

