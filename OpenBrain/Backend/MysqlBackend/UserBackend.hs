module OpenBrain.Backend.MysqlBackend.UserBackend () where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Database.HDBC as H
import Data.Maybe
import System.Time as T

import OpenBrain.Backend
import OpenBrain.Backend.MysqlBackend.Convertibles ()
import OpenBrain.Backend.MysqlBackend.Common
import OpenBrain.Backend.MysqlBackend.ProfileBackend ()
import OpenBrain.Common
import OpenBrain.Data.User
import OpenBrain.Data.Hash
import OpenBrain.Data.Karma

instance UserBackend MysqlBackend where
  login           b = withWConn (conn b) login'
  getUser         b = withWConn (conn b) getUser'
  hasUserWithId   b = withWConn (conn b) hasUserWithId'
  hasUserWithName b = withWConn (conn b) hasUserWithName'
  register        b = withWConn (conn b) register'
  delete          b = withWConn (conn b) delete'
  profileBackend    = CProfileBackend
  getUserCount    b = withWConn (conn b) getUserCount'
  getUserList     b = withWConn (conn b) getUserList'
  updateKarma     b = withWConn (conn b) updateKarma'
  updatePasswd    b = withWConn (conn b) updatePasswd'
  setAdmin        b = withWConn (conn b) setAdmin'

login' :: (IConnection conn) => conn -> UserName -> Hash -> MaybeT IO UserData
login' conn username hash = do
  rst <- liftIO $ quickQuery conn "SELECT userid, karma, creation, lastLogin, isAdmin FROM UserData WHERE username = ? AND password = ?" [toSql username, toSql hash]
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
      liftIO $ do
        t <- liftM toUTCTime getClockTime
        stmt <- prepare conn "UPDATE UserData SET lastLogin = ? WHERE userid = ?"
        execute stmt [toSql t, toSql (userid userdata)] >> commit conn
        return userdata{lastLogin = t}
    _ -> mzero

getUser' :: (IConnection conn, UserIdentifier ui) => conn -> ui -> MaybeT IO UserData
getUser' conn uid = do
  let userid = getUserId uid
  rst <- liftIO $ quickQuery conn "SELECT username, password, karma, creation, lastLogin, isAdmin FROM UserData WHERE userid = ?" [toSql userid]
  case rst of
    [[username', password', karma', creation', lastLogin', isAdmin']] -> return UserData {
        userid    = userid
      , username  = fromSql username'
      , password  = fromSql password'
      , karma     = fromSql karma'
      , creation  = fromSql creation'
      , lastLogin = fromSql lastLogin'
      , isAdmin   = fromSql isAdmin'
      }
    _ -> mzero

hasUserWithId' :: (IConnection conn, UserIdentifier ui) => conn -> ui -> IO Bool
hasUserWithId' conn uid = do
  rst <- quickQuery conn "SELECT COUNT(*) FROM UserData WHERE userid = ?" [toSql $ getUserId uid]
  case rst of
    [[count]] -> return $ (>0) (fromSql count :: Int)
    _         -> return False

hasUserWithName' :: (IConnection conn) => conn -> UserName -> MaybeT IO UserId
hasUserWithName' conn username = do
  rst <- liftIO $ quickQuery conn "SELECT userid FROM UserData WHERE username = ?" [toSql username]
  case rst of
    [[uid]] -> return $ fromSql uid
    _       -> mzero

register' :: (IConnection conn) => conn -> UserName -> Hash -> MaybeT IO UserData
register' conn username hash = do
  duplicate <- liftIOM isJust . runMaybeT $ hasUserWithName' conn username
  guard $ not duplicate
  liftIO $ do
    t <- liftM (toSql . toUTCTime) getClockTime
    stmt <- liftIO $ prepare conn "INSERT INTO UserData(username, password, creation, lastLogin, salt) VALUES (?, ?, ?, ?, '')"
    execute stmt [toSql username, toSql hash, t ,t] >> commit conn
  login' conn username hash

delete' :: (IConnection conn, UserIdentifier ui) => conn -> ui -> IO Bool
delete' conn uid = do
  stmt  <- prepare conn "DELETE FROM UserData WHERE userid = ?"
  rst   <- execute stmt [toSql $ getUserId uid]
  commit conn
  return $ rst > 0

getUserCount' :: (IConnection conn) => conn -> IO Int
getUserCount' conn = do
  rst <- quickQuery conn "SELECT COUNT(*) FROM UserData" []
  case rst of
    [[c]] -> return $ fromSql c
    _ -> return 0

getUserList' :: (IConnection conn) => conn -> Limit -> Offset -> IO [UserId]
getUserList' conn limit offset = do
  rst <- quickQuery conn "SELECT userid FROM UserData LIMIT ? OFFSET ?" [toSql limit, toSql offset]
  return $ concatMap go rst
  where
    go [uid]  = [fromSql uid]
    go _      = []

updateKarma' :: (IConnection conn, UserIdentifier ui) => conn -> ui -> (Karma -> Karma) -> IO ()
updateKarma' conn uid f = do
  let userid = getUserId uid
  rst <- quickQuery conn "SELECT karma FROM UserData WHERE userid = ?" [toSql userid]
  case rst of
    [[k]] -> do
      let k' = f $ fromSql k
      stmt <- prepare conn "UPDATE UserData SET karma = ? WHERE userid = ?"
      execute stmt [toSql k', toSql userid] >> commit conn
    _ -> return ()

updatePasswd' :: (IConnection conn, UserIdentifier ui) => conn -> ui -> Hash -> IO ()
updatePasswd' conn uid hash = do
  stmt <- prepare conn "UPDATE UserData SET password = ? WHERE userid = ?"
  execute stmt [toSql hash, toSql $ getUserId uid] >> commit conn

setAdmin' :: (IConnection conn, UserIdentifier ui) => conn -> ui -> Bool -> IO ()
setAdmin' conn uid admin = do
  stmt <- prepare conn "UPDATE UserData SET isAdmin = ? WHERE userid = ?"
  execute stmt [toSql admin, toSql $ getUserId uid] >> commit conn

