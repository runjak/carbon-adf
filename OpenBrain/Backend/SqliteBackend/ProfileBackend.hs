module OpenBrain.Backend.SqliteBackend.ProfileBackend (load) where
{- Provides the ProfileBackend for SqliteBackend.UserBackend. -}

import Control.Monad
import Database.HDBC as H

import OpenBrain.Backend
import OpenBrain.Backend.SqliteBackend.Convertibles ()
import OpenBrain.Backend.SqliteBackend.Schema (SnippetType(..))
import OpenBrain.Data.Profile
import OpenBrain.Data.User (UserId)

load :: (IConnection conn) => conn -> ProfileBackend
load conn = ProfileBackend {
    getProfileId        = getProfileId' conn
  , getProfile          = getProfile' conn
  , setAccessRule       = setAccessRule' conn
  , setName             = setName' conn
  , setAvatar           = setAvatar' conn
  , setLocations        = setLocations' conn
  , setWebsites         = setWebsites' conn
  , setEmails           = setEmails' conn
  , setInstantMessagers = setInstantMessagers' conn
  }

getProfileId' :: (IConnection conn) => conn -> UserId -> IO ProfileId
getProfileId' conn userid = do
  rst <- quickQuery conn "SELECT profileid FROM Profile WHERE userid = ? " [toSql userid]
  case rst of
    [[pid]] -> return $ fromSql pid
    _ -> do
      stmt <- prepare conn "INSERT INTO Profile(userid) VALUES (?)"
      state <- execute stmt [toSql userid]
      when (state <= 0) $ error "Could not create new profileid in SqliteBackend."
      commit conn >> getProfileId' conn userid

getProfile' :: (IConnection conn) => conn -> ProfileId -> IO (Maybe Profile)
getProfile' conn profileid = do
  pdata <- quickQuery' conn "SELECT userid, accessRule, avatar FROM Profile WHERE profileid = ?" [toSql profileid]
  ndata <- quickQuery' conn "SELECT prefix, foreName, middleName, familyName, suffix FROM Name WHERE profileif = ?" [toSql profileid]
  ldata <- quickQuery' conn "SELECT street, city, state, land, zipCode, note FROM Location WHERE profileid = ?" [toSql profileid]
  let locations' = concatMap mkLocations ldata
  mname <- case ndata of
    [[p, fon, mn, fan, s]] -> return $ Just $ Name (fromSql p) (fromSql fon) (fromSql mn) (fromSql fan) (fromSql s)
    _ -> return Nothing
  websites' <- getProfileSnippets Website
  emails'   <- getProfileSnippets Email
  ims       <- getProfileSnippets InstantMessager
  case pdata of
    [[userid', accessRule', avatar']] -> return . Just $ Profile {
        profileId         = profileid
      , userId            = fromSql userid'
      , accessRule        = fromSql accessRule'
      , name              = mname
      , avatar            = fromSql avatar'
      , locations         = locations'
      , websites          = websites'
      , emails            = emails'
      , instantMessagers  = ims
      }
    _ -> return Nothing
  where
    getProfileSnippets :: SnippetType -> IO [ProfileSnippet]
    getProfileSnippets st = do
      rst <- quickQuery' conn "SELECT title, description, target FROM ProfileSnippet WHERE profileid = ? AND snippetType = ?" [toSql profileid, toSql st]
      return $ concatMap go rst
      where
        go [ti, de, ta] = [ProfileSnippet (fromSql ti) (fromSql de) (fromSql ta)]
        go _ = []
    mkLocations :: [SqlValue] -> [Location]
    mkLocations [street', city', state', land', zipCode', note'] =
      [Location (fromSql street') (fromSql city') (fromSql state') (fromSql land') (fromSql zipCode') (fromSql note')]
    mkLocations _ = []

setAccessRule' :: (IConnection conn) => conn -> ProfileId -> AccessRule -> IO ()
setAccessRule' conn profileid accessrule = do
  stmt <- prepare conn "UPDATE Profile SET accessRule = ? WHERE profileid = ?"
  execute stmt [toSql accessrule, toSql profileid] >> commit conn

setName' :: (IConnection conn) => conn -> ProfileId -> Maybe Name -> IO ()
setName' conn profileid mname = do
  quickQuery conn "DELETE FROM Name WHERE profileid = ?" [toSql profileid]
  case mname of
    Nothing -> return ()
    (Just n) -> do
      stmt <- prepare conn "INSERT INTO Name(profileid, prefix, foreName, middleName, familyName, suffix) VALUES (?, ?, ?, ?, ?, ?)"
      execute stmt [toSql profileid, toSql $ prefix n, toSql $ foreName n, toSql $ middleName n, toSql $ familyName n, toSql $ suffix n]
      commit conn

setAvatar' :: (IConnection conn) => conn -> ProfileId -> Maybe String -> IO ()
setAvatar' conn profileid mavatar = do
  stmt <- prepare conn "UPDATE Profile SET avatar = ? WHERE profileid = ?"
  execute stmt [toSql mavatar, toSql profileid] >> commit conn

setLocations' :: (IConnection conn) => conn -> ProfileId -> [Location] -> IO ()
setLocations' conn profileid locations = do
  quickQuery conn "DELETE FROM Location WHERE profileid = ?" [toSql profileid]
  stmt <- prepare conn "INSERT INTO Location(profileid, street, city, state, land, zipCode, note) VALUES (?, ?, ?, ?, ?, ?, ?)"
  executeMany stmt (map go locations) >> commit conn
  where
    go :: Location -> [SqlValue]
    go l = [toSql profileid, toSql $ street l, toSql $ city l, toSql $ state l, toSql $ land l, toSql $ zipCode l, toSql $ note l]

setWebsites' :: (IConnection conn) => conn -> ProfileId -> [ProfileSnippet] -> IO ()
setWebsites' conn profileid websites = setProfileSnippets conn profileid Website websites

setEmails' :: (IConnection conn) => conn -> ProfileId -> [ProfileSnippet] -> IO ()
setEmails' conn profileid emails = setProfileSnippets conn profileid Email emails

setInstantMessagers' :: (IConnection conn) => conn -> ProfileId -> [ProfileSnippet] -> IO ()
setInstantMessagers' conn profileid ims = setProfileSnippets conn profileid InstantMessager ims

setProfileSnippets :: (IConnection conn) => conn -> ProfileId -> SnippetType -> [ProfileSnippet] -> IO ()
setProfileSnippets conn profileid snippettype snippets = do
  quickQuery conn "DELETE FROM ProfileSnippet WHERE snippetType = ? AND profileid = ?" [toSql snippettype, toSql profileid]
  stmt <- prepare conn "INSERT INTO ProfileSnippet(profileid, title, description, target, snippetType) VALUES (?, ?, ?, ?, ?)"
  executeMany stmt $ map (\s -> [toSql profileid, toSql $ title s, toSql $ description s, toSql $ target s, toSql snippettype]) snippets
  commit conn
