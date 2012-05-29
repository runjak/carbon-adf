module OpenBrain.Backend.RamBackend (load) where
{-
  This module provides everything to load a working RamBackend.
  The RamBackend stores everything in ram and forgets when the application stops.
-}
import OpenBrain.Backend
import qualified OpenBrain.Backend.RamBackend.KarmaBackend as K
import OpenBrain.Backend.RamBackend.UserBackend
import OpenBrain.Config
import OpenBrain.Config.Karma (KarmaConfig(..))
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

load :: Config -> IO Backend
load config = do
  userRamData <- mkUserRamData
  userBackend <- loadUserBackend userRamData
  karmaBackend  <- loadKarmaBackend userRamData $ karmaConfig config
  return $ Backend userBackend undefined

mkUserRamData :: IO UserRamData
mkUserRamData = do
  userIdMap   <- atomically $ newTVar M.empty
  userNameMap <- atomically $ newTVar M.empty
  return (userIdMap, userNameMap)

loadUserBackend :: UserRamData -> IO UserBackend
loadUserBackend userRamData = do
  return $ UserBackend{
      login           = rLogin userRamData
    , getUser         = rGetUser userRamData
    , hasUserWithId   = rHasUserWithId userRamData
    , hasUserWithName = rHasUserWithName userRamData
    , register        = rRegister userRamData
    , delete          = rDelete userRamData
    , getProfile      = undefined -- FIXME IMPLEMENT
    , setProfile      = undefined -- FIXME IMPLEMENT
  }

loadKarmaBackend :: UserRamData -> KarmaConfig -> IO KarmaBackend
loadKarmaBackend userRamData karmaConfig = do
  return $ KarmaBackend {
      karmaDeleteUser = K.deleteUser userRamData karmaConfig
    , karmaEditUser   = K.editUser userRamData karmaConfig
  }
