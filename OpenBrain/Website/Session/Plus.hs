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
chkSession = do
  liftMaybe =<< (lift . Session.chkSession) =<< gets backend
  `mplus` rqError "Invalid session."

-- | mzero on fail
chkAction :: OBW UserId
chkAction = do
  liftMaybe =<< (lift . Session.chkAction) =<< gets backend
  `mplus` rqError "Invalid action."

dropSession :: OBW ()
dropSession = (lift . Session.dropSession) =<< gets backend
