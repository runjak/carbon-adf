module OpenBrain.Backend.RamBackend.KarmaBackend where
{- Provides the KarmaBackend for the RamBackend -}

import OpenBrain.Backend.RamBackend.UserBackend (UserRamData)
import OpenBrain.Config
import OpenBrain.User.Data
import OpenBrain.User.Karma

import Control.Concurrent.STM as STM
import Control.Monad
import qualified Data.Map as M

maxKarma :: UserRamData -> IO Karma
maxKarma = atomically . liftM (maximum . map karma . M.elems) . readTVar . fst

deleteUser :: UserRamData -> KarmaConfig -> IO Karma
deleteUser urd kc = liftM (satisfiesRatio $ ratioDeleteUser kc) $ maxKarma urd

editUser :: UserRamData -> KarmaConfig -> IO Karma
editUser urd kc = liftM (satisfiesRatio $ ratioEditUser kc) $ maxKarma urd
