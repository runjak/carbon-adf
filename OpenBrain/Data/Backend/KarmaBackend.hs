{-# LANGUAGE GADTs #-}
module OpenBrain.Data.Backend.KarmaBackend where

import OpenBrain.Data

data KBackendReq r where
  KarmaDeleteUser :: KBackendReq Karma
  KarmaEditUser   :: KBackendReq Karma