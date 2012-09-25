module OpenBrain.Backend.PostgreSQLBackend.UserBackend (getUser') where

import Control.Exception as Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Database.HDBC as H
import Data.Maybe
import System.Time as T

import OpenBrain.Backend
import OpenBrain.Backend.PostgreSQLBackend.Convertibles ()
import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.Types
import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Data.User
import OpenBrain.Data.Hash
import OpenBrain.Data.Karma
import OpenBrain.Data.Salt

instance UserBackend PostgreSQLBackend where
  login           b = withWConn (conn b) login'
  getUser         b = withWConn (conn b) getUser'
  hasUserWithId   b = withWConn (conn b) hasUserWithId'
  hasUserWithName b = withWConn (conn b) hasUserWithName'
  register        b = withWConn (conn b) register'
  delete          b = withWConn (conn b) delete'
  getUserCount    b = withWConn (conn b) getUserCount'
  getUserList     b = withWConn (conn b) getUserList'
  updateKarma     b = withWConn (conn b) updateKarma'
  updatePasswd    b = withWConn (conn b) updatePasswd'
  setAdmin        b = withWConn (conn b) setAdmin'
  setProfile      b = withWConn (conn b) setProfile'

login' :: (IConnection conn) => conn -> UserName -> Hash -> MaybeT IO UserData
login' conn username hash = do
  let q = "SELECT userid, karma, creation, CURRENT_TIMESTAMP, isadmin, profile FROM \"UserData\" WHERE username = ? AND password = ?"
  rst <- liftIO $ quickQuery' conn q [toSql username, toSql hash]
  case rst of
    [[userid', karma', creation', lastLogin', isAdmin', profile']] -> do
      let userdata = UserData {
          userid    = fromId $ fromSql userid'
        , username  = username
        , password  = hash
        , karma     = fromSql karma'
        , creation  = fromSql creation'
        , lastLogin = fromSql lastLogin'
        , isAdmin   = fromSql isAdmin'
        , profile   = liftM fromId $ fromSql profile'
        }
      liftIO $ do
        quickQuery' conn "UPDATE \"UserData\" SET lastlogin = CURRENT_TIMESTAMP WHERE userid = ?" [toSql . toId $ userid userdata]
        return userdata
    _ -> mzero

getUser' :: (IConnection conn) => conn -> UserId -> MaybeT IO UserData
getUser' conn uid = do
  let q = "SELECT username, password, karma, creation, lastlogin, isadmin, profile FROM \"UserData\" WHERE userid = ?"
  rst <- liftIO $ quickQuery' conn q [toSql $ toId uid]
  case rst of
    [[username', password', karma', creation', lastLogin', isAdmin', profile']] -> return UserData {
        userid    = uid
      , username  = fromSql username'
      , password  = fromSql password'
      , karma     = fromSql karma'
      , creation  = fromSql creation'
      , lastLogin = fromSql lastLogin'
      , isAdmin   = fromSql isAdmin'
      , profile   = liftM fromId $ fromSql profile'
      }
    _ -> mzero

hasUserWithId' :: (IConnection conn) => conn -> UserId -> IO Bool
hasUserWithId' conn uid = do
  rst <- quickQuery' conn "SELECT COUNT(*) FROM \"UserData\" WHERE userid = ?" [toSql $ toId uid]
  case rst of
    [[count]] -> return $ (>0) (fromSql count :: Int)
    _         -> return False

hasUserWithName' :: (IConnection conn) => conn -> UserName -> MaybeT IO UserId
hasUserWithName' conn username = do
  rst <- liftIO $ quickQuery' conn "SELECT userid FROM \"UserData\" WHERE username = ?" [toSql username]
  case rst of
    [[uid]] -> return . fromId $ fromSql uid
    _       -> mzero

register' :: (IConnection conn) => conn -> UserName -> Hash -> Salt -> MaybeT IO UserData
register' conn username hash salt = do
  duplicate <- liftIOM isJust . runMaybeT $ hasUserWithName' conn username
  guard $ not duplicate
  liftIO $ do
    insert <- prepare conn "INSERT INTO \"UserData\" (username, password, salt) VALUES (?, ?, ?)"
    execute insert [toSql username, toSql hash, toSql salt]
    commit conn
  login' conn username hash

delete' :: (IConnection conn) => conn -> UserId -> IO Bool
delete' conn uid = do
  stmt  <- prepare conn "DELETE FROM \"UserData\" WHERE userid = ?"
  rst   <- execute stmt [toSql $ toId uid]
  commit conn
  return $ rst > 0

getUserCount' :: (IConnection conn) => conn -> IO Int
getUserCount' conn = do
  rst <- quickQuery' conn "SELECT COUNT(*) FROM \"UserData\"" []
  case rst of
    [[c]] -> return $ fromSql c
    _ -> return 0

getUserList' :: (IConnection conn) => conn -> Limit -> Offset -> IO [UserId]
getUserList' conn limit offset = do
  rst <- quickQuery' conn "SELECT userid FROM \"UserData\" LIMIT ? OFFSET ?" [toSql limit, toSql offset]
  return $ concatMap go rst
  where
    go [uid]  = [fromId $ fromSql uid]
    go _      = []

updateKarma' :: (IConnection conn) => conn -> UserId -> (Karma -> Karma) -> IO ()
updateKarma' conn uid f = do
  let userid = toId uid
  rst <- quickQuery' conn "SELECT karma FROM \"UserData\" WHERE userid = ?" [toSql userid]
  case rst of
    [[k]] -> do
      let k' = f $ fromSql k
      stmt <- prepare conn "UPDATE \"UserData\" SET karma = ? WHERE userid = ?"
      execute stmt [toSql k', toSql userid] >> commit conn
    _ -> return ()

updatePasswd' :: (IConnection conn) => conn -> UserId -> Hash -> IO ()
updatePasswd' conn uid hash = do
  stmt <- prepare conn "UPDATE \"UserData\" SET password = ? WHERE userid = ?"
  execute stmt [toSql hash, toSql $ toId uid] >> commit conn

setAdmin' :: (IConnection conn) => conn -> UserId -> Bool -> IO ()
setAdmin' conn uid admin = do
  stmt <- prepare conn "UPDATE \"UserData\" SET isadmin = ? WHERE userid = ?"
  execute stmt [toSql admin, toSql $ toId uid] >> commit conn

setProfile' :: (IConnection conn) => conn -> UserId -> Maybe InformationId -> IO ()
setProfile' conn uid iid = do
  let _iid  = toSql $ liftM toId iid
      q     = "UPDATE \"UserData\" SET profile = ? WHERE userid = ?"
  quickQuery' conn q [_iid, toSql $ toId uid] >> commit conn

