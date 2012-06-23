module OpenBrain.Backend.Plus where
{- Helpfull functions that operate on Website.Backend and are nice to have. -}

import Control.Monad
import Control.Monad.Trans (liftIO)

import OpenBrain.Backend
import OpenBrain.Data.User

getUserByName :: UserBackend -> UserName -> IO (Maybe UserData)
getUserByName ub uname = do
  muid <- hasUserWithName ub uname
  case muid of
    Nothing -> return Nothing
    (Just uid) -> getUser ub uid

