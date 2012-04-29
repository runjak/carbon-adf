{-# LANGUAGE DoAndIfThenElse, GeneralizedNewtypeDeriving #-}
module OpenBrain.Website.Session (
    SessionKey
  , Session(actionKey, userId)
  , SessionManager(..)
  , newSessionManager
) where
{-
  Session management for clients.
-}

import OpenBrain.Config
import OpenBrain.User.Data (UserId)
import OpenBrain.User.Hash (Hash, hash)

import Control.Concurrent.STM as STM
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import System.Random (StdGen, newStdGen, random)
import System.Time (ClockTime, getClockTime, TimeDiff(..), addToClockTime)

newtype SessionKey = SK Hash deriving (Show, Ord, Eq)

type ActionKey = Integer
data Session = Session {
    actionKey     :: ActionKey  -- used to prevent CSRF
  , sessionSecret :: Integer    -- not exported
  , userId        :: UserId
}

mkSessionKey :: Session -> SessionKey
mkSessionKey s = SK . hash $ concat [(show $ sessionSecret s),(show $ userId s)]

data SessionManager = SessionManager {
  {-| Initializes a new Sesison for a given UserId. -}
    mkSession     :: UserId -> IO (SessionKey, Session)
  {-| Returns a Session for a given Sessionkey if possible. -}
  , getSession    :: SessionKey -> IO (Maybe Session)
  {-|
    Returns Nothing on invalid ActionKey.
    If a Session is returned, it already has a new ActionKey.
  -}
  , performAction :: SessionKey -> ActionKey -> IO (Maybe Session)
}

newSessionManager :: Config -> IO SessionManager
newSessionManager config = do
  ms <- newManagerState
  return $ SessionManager {
      mkSession     = mkSession' ms
    , getSession    = (\sk -> chkTimeouts config ms >> getSession' ms sk)
    , performAction = performAction' ms
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

mkSession' :: ManagerState -> UserId -> IO (SessionKey, Session)
mkSession' ms uid = do
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
    return (sKey, s)

getSession' :: ManagerState -> SessionKey -> IO (Maybe Session)
getSession' ms sk = do
  t <- getClockTime
  atomically $ do
    mS <- liftM (M.lookup sk) . readTVar $ sessions ms
    if isJust mS
    then do
      ts <- readTVar $ timeOuts ms
      writeTVar (timeOuts ms) . (M.insert t sk) $ M.filter (/= sk) ts
      return mS
    else return Nothing

performAction' :: ManagerState -> SessionKey -> ActionKey -> IO (Maybe Session)
performAction' ms sk ak = atomically $ do
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
