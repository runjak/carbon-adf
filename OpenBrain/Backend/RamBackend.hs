{-# LANGUAGE DoAndIfThenElse #-}
module OpenBrain.Backend.RamBackend (load) where
{-
  This module provides everything to load a working RamBackend.
  The RamBackend stores everything in ram and forgets when the application stops.
-}
import OpenBrain.Backend
import OpenBrain.User.Data
import OpenBrain.User.Hash (Hash)
import OpenBrain.User.Karma (newKarma)

import Control.Concurrent.STM as STM
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import System.Time (getClockTime)
import qualified System.Time as T

load :: IO Backend
load = liftM Backend loadUserBackend

type UserRamData = (TVar (Map UserId UserData), TVar (Map UserName UserData))
loadUserBackend :: IO UserBackend
loadUserBackend = do
  userIdMap   <- atomically $ newTVar M.empty
  userNameMap <- atomically $ newTVar M.empty
  let userRamData = (userIdMap, userNameMap)
  return $ UserBackend{
      login           = rLogin userRamData
    , getUser         = rGetUser userRamData
    , hasUserWithId   = rHasUserWithId userRamData
    , hasUserWithName = rHasUserWithName userRamData
    , register        = rRegister userRamData
    , delete          = rDelete userRamData
  }

rLogin :: UserRamData -> UserName -> Hash -> IO (Maybe UserData)
rLogin (_, userNameMap) un h = atomically $ do
  unm <- readTVar userNameMap
  return $ do
    u <- M.lookup un unm
    guard . (h ==) $ password u
    return u

rGetUser :: UserRamData -> UserId -> IO (Maybe UserData)
rGetUser (userIdMap, _) uid = atomically . liftM (M.lookup uid) $ readTVar userIdMap

rHasUserWithId :: UserRamData -> UserId -> IO Bool
rHasUserWithId (userIdMap, _) uid = atomically . liftM (M.member uid) $ readTVar userIdMap

rHasUserWithName :: UserRamData -> UserName -> IO Bool
rHasUserWithName (_, userNameMap) un = atomically . liftM (M.member un) $ readTVar userNameMap

rRegister :: UserRamData -> UserName -> Hash -> IO (Maybe UserData)
rRegister (userIdMap, userNameMap) un h = do
  t <- getClockTime
  atomically $ do
    uim <- readTVar userIdMap
    unm <- readTVar userNameMap
    if M.member un unm
    then return $ Nothing
    else do
      let uid = maximum . ([toUserId 0]++) $ M.keys uim
      let u = UserData {
          userid    = uid
        , username  = un
        , password  = h
        , karma     = newKarma
        , creation  = t
        , lastLogin = t
      }
      let uim' = M.insert uid u uim
      let unm' = M.insert un u unm
      writeTVar userIdMap uim'
      writeTVar userNameMap unm'
      return $ Just u

rDelete :: UserRamData -> UserId -> IO Bool
rDelete (userIdMap, userNameMap) uid = atomically $ do
  uim <- readTVar userIdMap
  unm <- readTVar userNameMap
  let u = M.lookup uid uim
  if isJust u
  then do
    let un = username $ fromJust u
    let uim' = M.delete uid uim
    let unm' = M.delete un unm
    writeTVar userIdMap uim'
    writeTVar userNameMap unm'
    return True
  else return False
