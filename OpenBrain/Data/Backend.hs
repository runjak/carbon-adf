{-# LANGUAGE GADTs #-}
module OpenBrain.Data.Backend(
    BackendReq(..)
  , LiftBackend(..)
  , delete, delete'
  , getUserByName, getUsers
  , module Data
  , module GBackend
  , module IBackend
  , module KBackend
  , module RBackend
  , module SShaker
  , module SManagement
  , module UBackend
)where

import Control.Monad
import Data.Maybe

import OpenBrain.Data                            as Data
import OpenBrain.Data.Backend.GeneralBackend     as GBackend
import OpenBrain.Data.Backend.InformationBackend as IBackend
import OpenBrain.Data.Backend.KarmaBackend       as KBackend
import OpenBrain.Data.Backend.RelationBackend    as RBackend
import OpenBrain.Data.Backend.SaltShaker         as SShaker
import OpenBrain.Data.Backend.SessionManagement  as SManagement
import OpenBrain.Data.Backend.UserBackend        as UBackend

data BackendReq r where
  GeneralBackend     :: GBackendReq    r -> BackendReq r
  InformationBackend :: IBackendReq    r -> BackendReq r
  KarmaBackend       :: KBackendReq    r -> BackendReq r
  RelationBackend    :: RBackendReq    r -> BackendReq r
  SaltShaker         :: SaltShakerReq  r -> BackendReq r
  SessionManagement  :: SManagementReq r -> BackendReq r
  UserBackend        :: UBackendReq    r -> BackendReq r
  Backendλ           :: BackendReq     p -> (p -> BackendReq r) -> BackendReq r
  -- | U+039b: Λ, U+03bb: λ

{-| Requests that can become BackendRequests |-}
class LiftBackend b where
  liftB :: b r -> BackendReq r
instance LiftBackend BackendReq where
  liftB = id
instance LiftBackend GBackendReq where
  liftB = GeneralBackend
instance LiftBackend IBackendReq where
  liftB = InformationBackend
instance LiftBackend KBackendReq where
  liftB = KarmaBackend
instance LiftBackend RBackendReq where
  liftB = RelationBackend
instance LiftBackend SaltShakerReq where
  liftB = SaltShaker
instance LiftBackend SManagementReq where
  liftB = SessionManagement
instance LiftBackend UBackendReq where
  liftB = UserBackend

{-| Oh I simply love Monads: |-}
instance Monad BackendReq where
  return  = liftB . Nop
  m >>= f = Backendλ m f

-- | Requests on top of BackendReq that use Backendλ:
delete :: UserId -> Heir -> BackendReq Bool
delete uid heir
  | uid == heir = liftB $ Nop False
  | otherwise   = liftB $ Delete uid heir

getUserByName :: UserName -> BackendReq (Maybe UserData)
getUserByName uname = do
  mUid <- liftB $ HasUserWithName uname 
  case mUid of
    Nothing    -> liftB $ Nop Nothing
    (Just uid) -> liftB $ GetUser uid

getUsers :: [UserId] -> BackendReq [UserData]
getUsers = liftM catMaybes . mapM (liftB . GetUser)

delete' :: UserId -> BackendReq Bool
delete' uid = liftB . Delete uid =<< liftB GetNobody
