{-# LANGUAGE GADTs #-}
module OpenBrain.Data.Backend.SaltShaker where

import OpenBrain.Data

data SaltShakerReq r where
  GetSalt :: UserId -> SaltShakerReq Salt
