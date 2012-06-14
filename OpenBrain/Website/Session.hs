{-# LANGUAGE DoAndIfThenElse, GeneralizedNewtypeDeriving #-}
module OpenBrain.Website.Session (
    Session(actionKey, userId)
  , actionKeyParameter
  , SessionManager(..)
  , newSessionManager
) where
{-
  Session management for clients.
  With login a client provides a Hash.
  With each following action the client provides the cleartext for the hash
  and a new hash. This legitimates the action by a secret the client knows.
  Therefore we store triples of UserId, Hashcode and a Timestamp used for session timeouts.
  The client delivers it's actionkey via cookie and always set's a new one.
  
  With a login a client gets a cookie with a hashcode.
  This hashcode is used to verify that the client is logged in as a certain user.
-}

import Control.Concurrent.STM as STM
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Happstack.Server as S hiding (Session, Cookie(..))
import qualified Happstack.Server as Cookie (Cookie(..), CookieLife(..))
import System.Random (StdGen, newStdGen, random)
import System.Time (ClockTime, getClockTime, TimeDiff(..), addToClockTime)

import OpenBrain.Config
import OpenBrain.Data.User (UserId)
import OpenBrain.Data.Hash (Hash, hash', fromHash, toHash)

newtype SessionKey = SK Hash deriving (Show, Ord, Eq)

type ActionKey = Integer
data Session = Session {
    actionKey     :: ActionKey  -- used to prevent CSRF
  , sessionSecret :: Integer    -- not exported
  , userId        :: UserId
}

mkSessionKey :: Session -> SessionKey
mkSessionKey s = SK . hash' $ concat [(show $ sessionSecret s),(show $ userId s)]

cookieName :: String
cookieName = "SessionKey"

{-| This is required for chkAction -}
actionKeyParameter :: String
actionKeyParameter = "ActionKey"

data SessionManager = SessionManager {
  {-| Initializes a new Session for a given UserId. -}
    mkSession   :: UserId -> ServerPartT IO ()
  {-| Fetches the UserId from a running session, fails with mzero -}
  , getSession  :: ServerPartT IO Session
  {-| Similar to getUserId, but also checks for a correct actionKeyParameter.
      The returned Session will have a new ActionKey set. -}
  , chkAction   :: ServerPartT IO Session
  {-| Expires the cookie that maybe set. -}
  , dropSession :: ServerPartT IO ()
}

newSessionManager :: Config -> IO SessionManager
newSessionManager config = do
  ms <- newManagerState
  return $ SessionManager {
      mkSession   = mkSession' ms
    , getSession  = getSession' ms
    , chkAction   = chkAction' ms
    , dropSession = dropSession' ms
  }

type LastAction = ClockTime
data ManagerState = MS {
    sessions :: TVar (Map SessionKey Session)
  , timeOuts :: TVar (Map LastAction SessionKey)
  , randSrc  :: TVar StdGen
}

newManagerState :: IO ManagerState
newManagerState = do
  gen <- newStdGen
  atomically $ do
    s <- newTVar M.empty
    t <- newTVar M.empty
    r <- newTVar gen
    return $ MS s t r

getRandom :: ManagerState -> STM Integer
getRandom ms = do
  gen <- readTVar $ randSrc ms
  let (r, gen') = random gen
  writeTVar (randSrc ms) gen'
  return r

chkTimeouts :: Config -> ManagerState -> IO ()
chkTimeouts conf ms = do
  let secs = negate . abs $ sessionTimeout conf
  t <- liftM (addToClockTime $ TimeDiff 0 0 0 0 0 secs 0) getClockTime
  atomically $ do
    (kill, keep) <- liftM (M.split t) . readTVar $ timeOuts ms
    writeTVar (timeOuts ms) keep
    s <- readTVar $ sessions ms
    writeTVar (sessions ms) $ foldl (flip M.delete) s $ M.elems kill

mkSession' :: ManagerState -> UserId -> ServerPartT IO ()
mkSession' ms uid = do
  sessionkey <- liftIO $ mkSession'' ms uid
  let cookie = mkCookie cookieName $ show sessionkey
  addCookie Cookie.Session cookie

mkSession'' :: ManagerState -> UserId -> IO SessionKey
mkSession'' ms uid = do
  t <- getClockTime
  atomically $ do
    aKey <- getRandom ms
    sSec <- getRandom ms
    let s = Session {
        actionKey     = aKey
      , sessionSecret = sSec
      , userId        = uid
    }
    let sKey = mkSessionKey s
    sessions' <- liftM (M.insert sKey s) . readTVar $ sessions ms
    writeTVar (sessions ms) sessions'
    timeOuts' <- liftM (M.insert t sKey) . readTVar $ timeOuts ms
    writeTVar (timeOuts ms) timeOuts'
    return (sKey)

getSession' :: ManagerState -> ServerPartT IO Session
getSession' ms = do
  sessionkey <- liftM (SK . toHash) $ lookCookieValue cookieName
  mSession <- liftIO $ getSession'' ms sessionkey
  case mSession of
    (Just session) -> return session
    Nothing -> mzero

getSession'' :: ManagerState -> SessionKey -> IO (Maybe Session)
getSession'' ms sk = do
  t <- getClockTime
  atomically $ do
    mS <- liftM (M.lookup sk) . readTVar $ sessions ms
    if isJust mS
    then do
      ts <- readTVar $ timeOuts ms
      writeTVar (timeOuts ms) . (M.insert t sk) $ M.filter (/= sk) ts
      return mS
    else return Nothing

chkAction' :: ManagerState -> ServerPartT IO Session
chkAction' ms = do
  sessionkey  <- liftM (SK . toHash) $ lookCookieValue cookieName
  actionkey   <- lookRead actionKeyParameter
  mSession    <- liftIO $ chkAction'' ms sessionkey actionkey
  case mSession of
    (Just session) -> return session
    Nothing -> mzero

chkAction'' :: ManagerState -> SessionKey -> ActionKey -> IO (Maybe Session)
chkAction'' ms sk ak = atomically $ do
  s <- readTVar $ sessions ms
  let mSes = M.lookup sk s
  if isJust mSes
  then do
    let ses = fromJust mSes
    if ak == actionKey ses
    then do
      ak' <- getRandom ms
      let ses' = ses{actionKey = ak'}
      writeTVar (sessions ms) $ M.insert sk ses' s
      return $ Just ses'
    else return Nothing
  else return Nothing

dropSession' :: ManagerState -> ServerPartT IO ()
dropSession' ms = do
  sessionkey  <- liftM (SK . toHash) $ lookCookieValue cookieName
  liftIO $ dropSession'' ms sessionkey
  expireCookie cookieName

dropSession'' :: ManagerState -> SessionKey -> IO ()
dropSession'' ms sk = atomically $ do
  s <- readTVar $ sessions ms
  t <- readTVar $ timeOuts ms
  writeTVar (sessions ms) $ M.delete sk s
  writeTVar (timeOuts ms) $ M.filter (/= sk) t
