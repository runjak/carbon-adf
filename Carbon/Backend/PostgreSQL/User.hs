{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Carbon.Backend.PostgreSQL.User where

import System.Random

import Carbon.Backend.PostgreSQL.Common
import Carbon.Data.Hash
import Carbon.Data.Id
import Carbon.Data.Salt

import qualified Carbon.Backend.PostgreSQL.Article as Article

addUser :: Username -> (Hash, Salt) -> IsAdmin -> Query (Either Error UserId)
addUser uname (h, s) isA conn = do
  let insert = "INSERT INTO users (username, hash, salt, isAdmin) VALUES (? ,? ,? , ?) RETURNING userid"
  rst <- quickQuery' conn insert [toSql uname, toSql h, toSql s, toSql isA]
  case rst of
    [[i]] -> return . Right $ fromSql i
    _ -> return . Left $ "Could not create new user with username " ++ uname

deleteUser :: UserId -> Heir -> Query (Maybe Error)
deleteUser uid heir conn = do
  let heirItems = "UPDATE items SET commitauthor = ? WHERE commitauthor = ?"
      delParticipants = "DELETE FROM discussion_participants WHERE userid = ?"
      delVoters = "DELETE FROM resultset_voters WHERE userid = ?"
      delUser = "DELETE FROM users WHERE userid = ?"
  quickQuery' conn heirItems [toSql heir, toSql uid]
  forM_ [delParticipants, delVoters, delUser] $ \q -> quickQuery' conn q [toSql uid]
  return Nothing

getNobody :: Query UserId
getNobody conn = do
  let getNobody = "SELECT userid FROM users WHERE username = 'Nobody'"
  rst <- quickQuery' conn getNobody []
  case rst of
    [[i]] -> return $ fromSql i
    _ -> do
      let mkNobody = "INSERT INTO users (username, hash, salt, isadmin) "
                  ++ "VALUES ('Nobody', 'impossible', '', False) RETURNING userid"
      rst' <- quickQuery' conn mkNobody []
      case rst' of
        [[i]] -> return $ fromSql i
        _ -> error "Could not create/find user Nobody."

getUser :: UserId -> Query (Either Error User)
getUser uid conn = do
  let q = "SELECT username, hash, salt, creationtime, lastlogin, isadmin, profile, sessionkey FROM users WHERE userid = ?"
  rst <- quickQuery' conn q [toSql uid]
  case rst of
    [[uname, h, s, ctime, ltime, isA, p, session]] -> return . Right $ User {
        userId       = uid
      , username     = fromSql uname
      , userhash     = toHash $ fromSql h
      , usersalt     = toSalt $ fromSql s
      , userCreation = fromSql ctime
      , lastLogin    = fromSql ltime
      , isAdmin      = fromSql isA
      , profile      = fromSql p
      , session      = fromSql session
      }
    _ -> return $ Left ("Could not find user with id:\t" ++ show uid)

hasUser :: Username -> Query (Maybe UserId)
hasUser uname conn = do
  let q = "SELECT userid FROM users WHERE username = ?"
  rst <- quickQuery' conn q [toSql uname]
  case rst of
    [[i]] -> return . Just $ fromSql i
    _ -> return Nothing

login :: UserId -> (Salt -> Hash) -> Query (Maybe SessionKey)
login uid f conn = do
  (r:rs) <- liftM randoms newStdGen
  let l = 10 + (r `mod` 11)
      (key :: String) = map (toEnum . (+ fromEnum 'a') . flip mod (fromEnum '~' - fromEnum 'a')) $ take l rs
      setKey = do
        quickQuery' conn "UPDATE users SET sessionkey = ? WHERE userid = ?" [toSql key, toSql uid]
        return $ Just key
  [[s, h]] <- quickQuery' conn "SELECT salt, hash FROM users WHERE userid = ?" [toSql uid]
  let valid = fromSql h == f (fromSql s)
  valid ? (setKey, return Nothing)

validate :: UserId -> SessionKey -> Query Bool
validate uid sKey conn = do
  let q = "SELECT sessionkey FROM users WHERE userid = ?"
  rst <- quickQuery' conn q [toSql uid]
  case rst of
    [[s]] -> return $ sKey == fromSql s
    _ -> return False

logout :: UserId -> Query ()
logout uid conn = void $ quickQuery' conn "UPDATE users SET sessionkey = NULL WHERE userid = ?" [toSql uid]

setAdmin :: UserId -> IsAdmin -> Query ()
setAdmin uid isA conn =
  let q = "UPDATE users SET isadmin = ? WHERE userid = ?"
  in void $ quickQuery' conn q [toSql isA, toSql uid]

setPasswd :: UserId -> (Salt -> Hash) -> Query ()
setPasswd uid f conn = do
  let getSalt = "SELECT salt FROM users WHERE userid = ?"
  [[s]] <- quickQuery' conn getSalt [toSql uid]
  let h = f . toSalt $ fromSql s
      setHash = "UPDATE users SET hash = ? WHERE userid = ?"
  void $ quickQuery' conn setHash [toSql h, toSql uid]

setProfile :: UserId -> Maybe ItemId -> Query ()
setProfile uid mI conn =
  let q = "UPDATE users SET profile = ? WHERE userid = ?"
  in void $ quickQuery' conn q [toSql mI, toSql uid]
