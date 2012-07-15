module OpenBrain.Website.Session.Plus where

import Happstack.Server as Server

import OpenBrain.Data.User
import OpenBrain.Website.Monad
import qualified OpenBrain.Website.Session as Session

mkSession :: UserId -> OBW ()
mkSession userId = do
  b <- gets backend
  lift $ Session.mkSession b userId

-- | mzero on fail
chkSession :: OBW UserId
chkSession = liftMaybe =<< (lift . Session.chkSession) =<< gets backend

-- | mzero on fail
chkAction :: OBW UserId
chkAction = liftMaybe =<< (lift . Session.chkAction) =<< gets backend

dropSession :: OBW ()
dropSession = (lift . Session.dropSession) =<< gets backend
