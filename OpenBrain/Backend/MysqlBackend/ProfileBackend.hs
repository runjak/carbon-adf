{-# LANGUAGE MultiParamTypeClasses #-}
module OpenBrain.Backend.MysqlBackend.ProfileBackend () where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Database.HDBC as H
import Data.Convertible.Base
import Data.Maybe

import OpenBrain.Backend
import OpenBrain.Backend.MysqlBackend.Convertibles ()
import OpenBrain.Backend.MysqlBackend.Common
import OpenBrain.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Profile

data SnippetType = Website | Email | InstantMessager
  deriving (Show, Read, Eq, Ord, Enum)
instance Convertible SnippetType SqlValue where
  safeConvert = Right . toSql . fromEnum
instance Convertible SqlValue SnippetType where
  safeConvert = Right . toEnum . fromSql

instance ProfileBackend MysqlBackend where
  getProfile b          = withWConn (conn b) getProfile'
  setAccessRule b       = withWConn (conn b) setAccessRule'
  setName b             = withWConn (conn b) setName'
  setAvatar b           = withWConn (conn b) setAvatar'
  setLocations b        = withWConn (conn b) setLocations'
  setWebsites b         = withWConn (conn b) setWebsites'
  setEmails b           = withWConn (conn b) setEmails'
  setInstantMessagers b = withWConn (conn b) setInstantMessagers'

getProfileId' :: (IConnection conn) => conn -> UserId -> IO ProfileId
getProfileId' conn uid = do
  let userid = toId uid
  rst <- quickQuery conn "SELECT profileid FROM Profile WHERE userid = ? " [toSql userid]
  case rst of
    [[pid]] -> return . fromId $ fromSql pid
    _ -> do
      stmt <- prepare conn "INSERT INTO Profile(userid) VALUES (?)"
      state <- execute stmt [toSql userid]
      when (state <= 0) $ error "Could not create new profileid in SqliteBackend."
      commit conn >> getProfileId' conn uid

getProfile' :: (IConnection conn) => conn -> UserId -> IO Profile
getProfile' conn uid = do
  pid <- getProfileId' conn uid
  let profileid = toId pid
  pdata <- liftIO $ quickQuery' conn "SELECT userid, accessRule, avatar FROM Profile WHERE profileid = ?" [toSql profileid]
  ndata <- liftIO $ quickQuery' conn "SELECT name_prefix, name_foreName, name_middleName, name_familyName, name_suffix FROM Profile WHERE profileid = ?" [toSql profileid]
  ldata <- liftIO $ quickQuery' conn "SELECT street, city, state, land, zipCode, note FROM Location WHERE profileid = ?" [toSql profileid]
  let locations = concatMap mkLocations ldata
      name      = mkName ndata
  websites  <- liftIO $ getProfileSnippets pid Website
  emails    <- liftIO $ getProfileSnippets pid Email
  ims       <- liftIO $ getProfileSnippets pid InstantMessager
  case pdata of
    [[userid', accessRule', avatar']] -> return Profile {
        profileId         = pid
      , userId            = fromId $ fromSql userid'
      , accessRule        = fromSql accessRule'
      , name              = (name /= emptyName) ? (Just name, Nothing)
      , avatar            = fromSql avatar'
      , locations         = locations
      , websites          = websites
      , emails            = emails
      , instantMessagers  = ims
      }
    _ -> return $ emptyProfile pid uid
  where
    getProfileSnippets :: ProfileId -> SnippetType -> IO [ProfileSnippet]
    getProfileSnippets profileid st = do
      rst <- quickQuery' conn "SELECT title, description, target FROM ProfileSnippet WHERE profileid = ? AND snippetType = ?" [toSql $ toId profileid, toSql st]
      return $ concatMap go rst
      where
        go [ti, de, ta] = [ProfileSnippet (fromSql ti) (fromSql de) (fromSql ta)]
        go _ = []
    
    mkLocations :: [SqlValue] -> [Location]
    mkLocations [street', city', state', land', zipCode', note'] =
      [Location (fromSql street') (fromSql city') (fromSql state') (fromSql land') (fromSql zipCode') (fromSql note')]
    mkLocations _ = []
    
    mkName :: [[SqlValue]] -> Name
    mkName [[p, fon, mn, fan, s]] = Name (fromSql p) (fromSql fon) (fromSql mn) (fromSql fan) (fromSql s)
    mkName _ = error "Could not produce Name in OpenBrain.Backend.MysqlBackend.ProfileBackend!"

setAccessRule' :: (IConnection conn) => conn -> ProfileId -> AccessRule -> IO ()
setAccessRule' conn pid accessrule = do
  let profileid = toId pid
  stmt <- prepare conn "UPDATE Profile SET accessRule = ? WHERE profileid = ?"
  execute stmt [toSql accessrule, toSql profileid] >> commit conn

setName' :: (IConnection conn) => conn -> ProfileId -> Maybe Name -> IO ()
setName' conn pid mname = do
  let profileid = toId pid
  (isJust mname) ? (wName conn profileid $ fromJust mname, dName conn profileid)
  where
    dName conn profileid = do
      stmt <- prepare conn "UPDATE Profile SET name_prefix = '', name_foreName = '', name_middleName = '', name_familyName = '', name_suffix = '' WHERE profileid = ?"
      execute stmt [toSql profileid] >> commit conn
    wName conn profileid n = do
      stmt <- prepare conn "UPDATE Profile SET name_prefix = ?, name_foreName = ?, name_middleName = ?, name_familyName = ?, name_suffix = ? WHERE profileid = ?"
      execute stmt [toSql $ prefix n, toSql $ foreName n, toSql $ middleName n, toSql $ familyName n, toSql $ suffix n, toSql profileid]
      commit conn

setAvatar' :: (IConnection conn) => conn -> ProfileId -> Maybe String -> IO ()
setAvatar' conn pid mavatar = do
  let profileid = toId pid
  stmt <- prepare conn "UPDATE Profile SET avatar = ? WHERE profileid = ?"
  execute stmt [toSql mavatar, toSql profileid] >> commit conn

setLocations' :: (IConnection conn) => conn -> ProfileId -> [Location] -> IO ()
setLocations' conn pid locations = do
  let profileid = toId pid
  quickQuery conn "DELETE FROM Location WHERE profileid = ?" [toSql profileid]
  stmt <- prepare conn "INSERT INTO Location(profileid, street, city, state, land, zipCode, note) VALUES (?, ?, ?, ?, ?, ?, ?)"
  executeMany stmt (map go locations) >> commit conn
  where
    go :: Location -> [SqlValue]
    go l = [toSql $ toId pid, toSql $ street l, toSql $ city l, toSql $ state l, toSql $ land l, toSql $ zipCode l, toSql $ note l]

setWebsites' :: (IConnection conn) => conn -> ProfileId -> [ProfileSnippet] -> IO ()
setWebsites' conn pid = setProfileSnippets conn pid Website

setEmails' :: (IConnection conn) => conn -> ProfileId -> [ProfileSnippet] -> IO ()
setEmails' conn pid = setProfileSnippets conn pid Email

setInstantMessagers' :: (IConnection conn) => conn -> ProfileId -> [ProfileSnippet] -> IO ()
setInstantMessagers' conn pid = setProfileSnippets conn pid InstantMessager

setProfileSnippets :: (IConnection conn) => conn -> ProfileId -> SnippetType -> [ProfileSnippet] -> IO ()
setProfileSnippets conn pid snippettype snippets = do
  let profileid = toId pid
  quickQuery conn "DELETE FROM ProfileSnippet WHERE snippetType = ? AND profileid = ?" [toSql snippettype, toSql profileid]
  stmt <- prepare conn "INSERT INTO ProfileSnippet(profileid, title, description, target, snippetType) VALUES (?, ?, ?, ?, ?)"
  executeMany stmt $ map (\s -> [toSql profileid, toSql $ title s, toSql $ description s, toSql $ target s, toSql snippettype]) snippets
  commit conn

