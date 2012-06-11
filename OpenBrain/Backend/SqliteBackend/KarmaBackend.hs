module OpenBrain.Backend.SqliteBackend.KarmaBackend (load) where
{- Provides the KarmaBackend for the SqliteBackend. -}

import Control.Monad
import Database.HDBC as H
import Data.Convertible.Base

import OpenBrain.Backend
import OpenBrain.Backend.SqliteBackend.Convertibles ()
import OpenBrain.Config.Karma
import OpenBrain.Data.Karma

-- Finding the highest current value of Karma in the system:
maxKarma :: IConnection conn => conn -> IO Karma
maxKarma conn = do
  rst <- quickQuery conn "SELECT DISTINCT karma FROM UserData ORDER BY karma DESC LIMIT 1" []
  case rst of
    [[k]] -> return $ fromSql k
    _     -> return $ toKarma 0

-- Producing the KarmaBackend for a Connection and a given KarmaConfig:
load :: IConnection conn => conn -> KarmaConfig -> KarmaBackend
load conn kc = KarmaBackend {
    karmaDeleteUser = liftM (satisfiesRatio $ ratioDeleteUser kc) $ maxKarma conn   
  , karmaEditUser   = liftM (satisfiesRatio $ ratioEditUser kc)   $ maxKarma conn
  }

