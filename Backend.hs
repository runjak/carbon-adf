{-# LANGUAGE ExistentialQuantification #-}
module Backend (
    Backend(..)
  , ProxyBackend
  , proxyBackend
  , UserControl(..)
  , proxyUserControl
) where
{-
  This module provides the Backend class that will be used to generate the website.
  The Backend will provide things like Userdata :P
-}
import Config
import User.Hash (Hash)
import User.Data (UserId, UserData(..), UserName)

class Backend b where
  getUsercontrol :: b -> IO ProxyUserControl

-- | To have a simple type for the Backend class.
data ProxyBackend = forall b . Backend b => PB b
instance Backend ProxyBackend where
  getUsercontrol (PB b) = getUsercontrol b

proxyBackend :: (Backend b) => b -> ProxyBackend
proxyBackend = PB

-- | Ways to manipulate user stuff
class UserControl u where
  -- | Tries to get UserData for correct login data.
  login       :: u -> UserName -> Hash -> IO (Maybe UserData)
  -- | Fetches UserData by UserId.
  getUserById :: u -> UserId -> IO (Maybe UserData)
  -- | A List of all UserId known.
  getUsers    :: u -> IO [UserData]

data ProxyUserControl = forall u . UserControl u => PUC u
instance UserControl ProxyUserControl where
  login (PUC u) = login u
  getUserById (PUC u) = getUserById u
  getUsers (PUC u) = getUsers u

proxyUserControl :: (UserControl u) => u -> ProxyUserControl
proxyUserControl = PUC
