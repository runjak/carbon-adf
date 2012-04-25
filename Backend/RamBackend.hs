module Backend.RamBackend (load) where
{-
  This module provides everything to load a working RamBackend.
  The RamBackend stores everything in ram and forgets when the application stops.
-}
import Backend
import User.Data

import Control.Concurrent.STM as STM
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M

type UserIdMap    = TVar (Map UserId UserData)
type UserNameMap  = TVar (Map UserName UserData)
data Users = Users {
    userIdMap :: UserIdMap
  , userNameMap :: UserNameMap
}

instance UserControl Users where
  login u name hash = atomically $ do
    unm <- readTVar $ userNameMap u
    return $ do
      userd <- M.lookup name unm
      guard (password userd == hash)
      return userd
  
  getUserById u id = atomically $ do
    uim <- readTVar $ userIdMap u
    return $ M.lookup id uim
  
  getUsers u = atomically $ do
    uim <- readTVar $ userIdMap u
    return $ M.elems uim

data Ram = Ram {
  users :: Users
}
emptyRam :: IO Ram
emptyRam = atomically $ do
  uim <- newTVar M.empty
  unm <- newTVar M.empty
  let u = Users uim unm
  return $ Ram u

instance Backend Ram where
  getUsercontrol = return . proxyUserControl . users

load :: IO ProxyBackend
load = liftM proxyBackend emptyRam
