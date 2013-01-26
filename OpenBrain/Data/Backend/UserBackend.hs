{-# LANGUAGE GADTs #-}
module OpenBrain.Data.Backend.UserBackend where

import OpenBrain.Data

data UBackendReq r where
  Login           :: UserName -> Hash -> UBackendReq (Maybe UserData) -- The Backend will update the lastLogin in UserData.
  GetUser         :: UserId -> UBackendReq (Maybe UserData)
  GetNobody       :: UBackendReq UserId
  HasUserWithId   :: UserId -> UBackendReq Bool
  HasUserWithName :: UserName -> UBackendReq (Maybe UserId)
  Register        :: UserName -> Hash -> Salt -> UBackendReq (Maybe UserData) -- The Backend will check for duplicate UserNames.
  Delete          :: UserId -> Heir -> UBackendReq Bool
  GetUserCount    :: UBackendReq Count
  GetUserList     :: Limit -> Offset -> UBackendReq [UserId]
  UpdateKarma     :: UserId -> (Karma -> Karma) -> UBackendReq ()
  UpdatePasswd    :: UserId -> Hash -> UBackendReq ()
  SetAdmin        :: UserId -> Bool -> UBackendReq ()
  SetProfile      :: UserId -> Maybe InformationId -> UBackendReq ()
