module OpenBrain.Backend.PostgreSQLBackend.Sql.UserBackend where

import Data.Maybe
import System.Time as T

import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.Types
import OpenBrain.Data.Id
import OpenBrain.Data.User
import OpenBrain.Data.Hash
import OpenBrain.Data.Karma
import OpenBrain.Data.Salt

login' :: (IConnection conn) => UserName -> Hash -> conn -> IO (Maybe UserData)
login' username hash conn = do
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
        return $ Just userdata
    _ -> return Nothing

getUser' :: (IConnection conn) => UserId -> conn -> IO (Maybe UserData)
getUser' uid conn = do
  let q = "SELECT username, password, karma, creation, lastlogin, isadmin, profile FROM \"UserData\" WHERE userid = ?"
  rst <- liftIO $ quickQuery' conn q [toSql $ toId uid]
  case rst of
    [[username', password', karma', creation', lastLogin', isAdmin', profile']] -> return $ Just UserData {
        userid    = uid
      , username  = fromSql username'
      , password  = fromSql password'
      , karma     = fromSql karma'
      , creation  = fromSql creation'
      , lastLogin = fromSql lastLogin'
      , isAdmin   = fromSql isAdmin'
      , profile   = liftM fromId $ fromSql profile'
      }
    _ -> return Nothing

getNobody' :: (IConnection conn) => conn -> IO UserId
getNobody' conn = do
  mUid <- hasUserWithName' "Nobody" conn
  case mUid of
    (Just uid) -> return uid
    Nothing    -> do
      let hash = toHash "Impossible"
      salt <- mkSalt
      register' "Nobody" hash salt conn
      getNobody' conn

hasUserWithId' :: (IConnection conn) => UserId -> conn -> IO Bool
hasUserWithId' uid conn = do
  rst <- quickQuery' conn "SELECT COUNT(*) FROM \"UserData\" WHERE userid = ?" [toSql $ toId uid]
  case rst of
    [[count]] -> return $ (>0) (fromSql count :: Int)
    _         -> return False

hasUserWithName' :: (IConnection conn) => UserName -> conn -> IO (Maybe UserId)
hasUserWithName' username conn = do
  rst <- liftIO $ quickQuery' conn "SELECT userid FROM \"UserData\" WHERE username = ?" [toSql username]
  case rst of
    [[uid]] -> return . Just . fromId $ fromSql uid
    _       -> return Nothing

register' :: (IConnection conn) => UserName -> Hash -> Salt -> conn -> IO (Maybe UserData)
register' username hash salt conn = do
  duplicate <- liftM isJust $ hasUserWithName' username conn
  guard $ not duplicate
  liftIO $ do
    insert <- prepare conn "INSERT INTO \"UserData\" (username, password, salt) VALUES (?, ?, ?)"
    execute insert [toSql username, toSql hash, toSql salt]
    return ()
  login' username hash conn

delete' :: (IConnection conn) => UserId -> Heir -> conn -> IO Bool
delete' uid heir conn = do
  let uid'  = toSql $ toId uid
      heir' = toSql $ toId heir
  -- Letting the heir become author of all Informations owned by the User.
  stmt  <- prepare conn "UPDATE \"Information\" SET author = ? WHERE author = ?"
  execute stmt [heir', uid']
  -- Deleting the User
  stmt  <- prepare conn "DELETE FROM \"UserData\" WHERE userid = ?"
  rst   <- execute stmt [uid']
  return $ rst > 0

getUserCount' :: (IConnection conn) => conn -> IO Int
getUserCount' conn = do
  rst <- quickQuery' conn "SELECT COUNT(*) FROM \"UserData\"" []
  case rst of
    [[c]] -> return $ fromSql c
    _ -> return 0

getUserList' :: (IConnection conn) => Limit -> Offset -> conn -> IO [UserId]
getUserList' limit offset conn = do
  rst <- quickQuery' conn "SELECT userid FROM \"UserData\" LIMIT ? OFFSET ?" [toSql limit, toSql offset]
  return $ concatMap go rst
  where
    go [uid]  = [fromId $ fromSql uid]
    go _      = []

updateKarma' :: (IConnection conn) => UserId -> (Karma -> Karma) -> conn -> IO ()
updateKarma' uid f conn = do
  let userid = toId uid
  rst <- quickQuery' conn "SELECT karma FROM \"UserData\" WHERE userid = ?" [toSql userid]
  case rst of
    [[k]] -> do
      let k' = f $ fromSql k
      stmt <- prepare conn "UPDATE \"UserData\" SET karma = ? WHERE userid = ?"
      void $ execute stmt [toSql k', toSql userid]
    _ -> return ()

updatePasswd' :: (IConnection conn) => UserId -> Hash -> conn -> IO ()
updatePasswd' uid hash conn = do
  stmt <- prepare conn "UPDATE \"UserData\" SET password = ? WHERE userid = ?"
  void $ execute stmt [toSql hash, toSql $ toId uid]

setAdmin' :: (IConnection conn) => UserId -> Bool -> conn -> IO ()
setAdmin' uid admin conn = do
  stmt <- prepare conn "UPDATE \"UserData\" SET isadmin = ? WHERE userid = ?"
  void $ execute stmt [toSql admin, toSql $ toId uid]

setProfile' :: (IConnection conn) => UserId -> Maybe InformationId -> conn -> IO ()
setProfile' uid iid conn = do
  let _iid  = toSql $ liftM toId iid
      q     = "UPDATE \"UserData\" SET profile = ? WHERE userid = ?"
  void $ quickQuery' conn q [_iid, toSql $ toId uid]

