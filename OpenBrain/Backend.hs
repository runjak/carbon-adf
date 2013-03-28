{-# LANGUAGE ExistentialQuantification, GADTs #-}
module OpenBrain.Backend(
    Backend(..)
  , CBackend(..)
  , module BReq
)where
{-
  This module provides the Backend class that will be used to generate the website.
  The Backend will provide things like Userdata :P
-}
import OpenBrain.Config       as Config
import OpenBrain.Data.Backend as BReq

class Backend b where
  process            :: b -> BackendReq     r -> IO r
  processGeneral     :: b -> GBackendReq    r -> IO r
  processInformation :: b -> IBackendReq    r -> IO r
  processKarma       :: b -> KBackendReq    r -> IO r
  processRelation    :: b -> RBackendReq    r -> IO r
  processSalt        :: b -> SaltShakerReq  r -> IO r
  processSession     :: b -> SManagementReq r -> IO r
  processUser        :: b -> UBackendReq    r -> IO r
  {-|
    Default implementation for process that distributes Requests
    and solves Requests of type Backendλ.
  |-}
  process b (GeneralBackend p)     = processGeneral     b p
  process b (InformationBackend p) = processInformation b p
  process b (KarmaBackend p)       = processKarma       b p
  process b (RelationBackend p)    = processRelation    b p
  process b (SaltShaker p)         = processSalt        b p
  process b (SessionManagement p)  = processSession     b p
  process b (UserBackend p)        = processUser        b p
  process b (Backendλ p f)         = process b p >>= process b . f

-- | A container for instances of Backend
data CBackend = forall b . Backend b => CBackend b
instance Backend CBackend where
  process            (CBackend b) = process            b
  processGeneral     (CBackend b) = processGeneral     b
  processInformation (CBackend b) = processInformation b
  processKarma       (CBackend b) = processKarma       b
  processRelation    (CBackend b) = processRelation    b
  processSalt        (CBackend b) = processSalt        b
  processSession     (CBackend b) = processSession     b
  processUser        (CBackend b) = processUser        b
