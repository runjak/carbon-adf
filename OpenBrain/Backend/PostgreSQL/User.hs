{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module OpenBrain.Backend.PostgreSQL.User where

import System.Random

import OpenBrain.Backend.PostgreSQL.Common
import OpenBrain.Data.Hash
import OpenBrain.Data.Id
import OpenBrain.Data.Salt

addUser :: Username -> (Hash,Salt) -> IsAdmin -> Query (Maybe UserId)
addUser uName (h,s) isA conn = do
  let insert = "INSERT INTO users (username, hash, salt, isadmin) VALUES (?, ?, ?, ?) RETURNING userid"
  rst <- quickQuery' conn insert [toSql uName, toSql h, toSql s, toSql isA]
  case rst of
    [[i]] -> return . Just . fromId $ fromSql i
    _ -> return Nothing

deleteUser :: UserId -> Heir -> Query ()
deleteUser uid heir conn = do
  let heirDesc   = "UPDATE descriptions SET userid = ? WHERE userid = ?"
      delWeights = "DELETE FROM weights WHERE userid = ?"
      delVotes   = "DELETE FROM voters WHERE userid = ?"
      delUser    = "DELETE FROM users WHERE userid = ?"
  quickQuery' conn heirDesc [toSql $ toId heir, toSql $ toId uid]
  mapM_ (\q -> quickQuery' conn q [toSql $ toId uid]) [delWeights, delVotes, delUser]

getNobody :: Query UserId
getNobody conn = do
  let getNobody = "SELECT userid FROM users WHERE username = 'Nobody'"
  rst <- quickQuery' conn getNobody []
  case rst of
    [[i]] -> return . fromId $ fromSql i
    _ -> do
      let mkNobody = "INSERT INTO users (username, hash, salt, isadmin) "
                  ++ "VALUES ('Nobody', 'impossible', '', False) RETURNING userid"
      rst' <- quickQuery' conn mkNobody []
      case rst' of
        [[i]] -> return . fromId $ fromSql i
        _ -> error "Could not create/find user Nobody."

getUser :: UserId -> Query User
getUser uid conn = do
  let q = "SELECT (username, hash, salt, creationtime, lastlogin, isadmin, profile, sessionkey) FROM users WHERE userid = ?"
  rst <- quickQuery' conn q [toSql $ toId uid]
  case rst of
    [[uname, h, s, ctime, ltime, isA, p, session]] -> return User {
        userId       = uid
      , username     = fromSql uname
      , userhash     = toHash $ fromSql h
      , usersalt     = toSalt $ fromSql s
      , userCreation = fromSql ctime
      , lastLogin    = fromSql ltime
      , isAdmin      = fromSql isA
      , profile      = liftM fromId $ fromSql p
      , session      = fromSql session
      }
    _ -> error ("Could not find user with id:\t" ++ show uid)

hasUser :: Username -> Query (Maybe UserId)
hasUser uname conn = do
  let q = "SELECT userid FROM users WHERE username = ?"
  rst <- quickQuery' conn q [toSql uname]
  case rst of
    [[i]] -> return . Just . fromId $ fromSql i
    _ -> return Nothing

login :: UserId -> (Salt -> Hash) -> Query (Maybe SessionKey)
login uid f conn = do
  (r:rs) <- liftM randoms newStdGen
  let l               = 10 + (r `mod` 11)
      (key :: String) = map (toEnum . (+ fromEnum 'a') . flip mod (fromEnum '~' - fromEnum 'a')) $ take l rs
      setKey          = do
        quickQuery' conn "UPDATE users SET sessionkey = ? WHERE userid = ?" [toSql key, toSql $ toId uid]
        return $ Just key
  [[s, h]] <- quickQuery' conn "SELECT salt, hash FROM users WHERE userid = ?" [toSql $ toId uid]
  let valid = fromSql h == f (fromSql s)
  valid ? (setKey, return Nothing)

validate :: UserId -> SessionKey -> Query Bool
validate uid sKey conn = do
  let q = "SELECT sessionkey FROM users WHERE userid = ?"
  rst <- quickQuery' conn q [toSql $ toId uid]
  case rst of
    [[s]] -> return $ sKey == fromSql s
    _ -> return False

logout :: UserId -> Query ()
logout uid conn = void $ quickQuery' conn "UPDATE users SET sessionkey = NULL WHERE userid = ?" [toSql $ toId uid]

setAdmin :: UserId -> IsAdmin -> Query ()
setAdmin uid isA conn =
  let q = "UPDATE users SET isadmin = ? WHERE userid = ?"
  in void $ quickQuery' conn q [toSql isA, toSql $ toId uid]

setPasswd :: UserId -> (Salt -> Hash) -> Query ()
setPasswd uid f conn = do
  let getSalt = "SELECT salt FROM users WHERE userid = ?"
  [[s]] <- quickQuery' conn getSalt [toSql $ toId uid]
  let h       = f . toSalt $ fromSql s
      setHash = "UPDATE users SET hash = ? WHERE userid = ?"
  void $ quickQuery' conn setHash [toSql h, toSql $ toId uid]

setProfile :: UserId -> Maybe ArticleId -> Query ()
setProfile uid mA conn =
  let q = "UPDATE users SET profile = ? WHERE userid = ?"
  in void $ quickQuery' conn q [toSql $ liftM toId mA, toSql $ toId uid]
