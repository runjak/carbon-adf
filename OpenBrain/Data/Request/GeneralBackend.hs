{-# LANGUAGE GADTs #-}
module OpenBrain.Data.Request.GeneralBackend where

import OpenBrain.Data

data GBackendReq r where
  Shutdown :: GBackendReq ()
  {- Necessary so that Backendλ can choose to to nothing, but return anything. -}
  Nop      :: r -> GBackendReq r
