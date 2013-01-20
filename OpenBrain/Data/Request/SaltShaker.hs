{-# LANGUAGE GADTs #-}
module OpenBrain.Data.Request.SaltShaker where

import OpenBrain.Data

data SaltShakerReq r where
  GetSalt :: UserId -> SaltShakerReq Salt
