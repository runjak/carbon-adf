module OpenBrain.Backend.Plus where
{- Helpfull extensions to OpenBrain.Backend -}

import Control.Monad
import Control.Monad.Trans.Maybe

import OpenBrain.Backend
import OpenBrain.Data.User

getUserByName :: (UserBackend b) => b -> UserName -> MaybeT IO UserData
getUserByName b name = getUser b =<< hasUserWithName b name

instance UserBackend CBackend where
  login           = login           . userBackend
  getUser         = getUser         . userBackend
  hasUserWithId   = hasUserWithId   . userBackend
  hasUserWithName = hasUserWithName . userBackend
  register        = register        . userBackend
  delete          = delete          . userBackend
  profileBackend  = profileBackend  . userBackend
  getUserCount    = getUserCount    . userBackend
  getUserList     = getUserList     . userBackend
  updateKarma     = updateKarma     . userBackend
  updatePasswd    = updatePasswd    . userBackend
  setAdmin        = setAdmin        . userBackend

instance ProfileBackend CUserBackend where
  getProfileId        = getProfileId        . profileBackend
  getProfile          = getProfile          . profileBackend
  setAccessRule       = setAccessRule       . profileBackend
  setName             = setName             . profileBackend
  setAvatar           = setAvatar           . profileBackend
  setLocations        = setLocations        . profileBackend
  setWebsites         = setWebsites         . profileBackend
  setEmails           = setEmails           . profileBackend
  setInstantMessagers = setInstantMessagers . profileBackend

instance ProfileBackend CBackend where
  getProfileId        = getProfileId        . userBackend
  getProfile          = getProfile          . userBackend
  setAccessRule       = setAccessRule       . userBackend
  setName             = setName             . userBackend
  setAvatar           = setAvatar           . userBackend
  setLocations        = setLocations        . userBackend
  setWebsites         = setWebsites         . userBackend
  setEmails           = setEmails           . userBackend
  setInstantMessagers = setInstantMessagers . userBackend
