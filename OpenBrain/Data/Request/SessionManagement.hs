{-# LANGUAGE GADTs #-}
module OpenBrain.Data.Request.SessionManagement where

import OpenBrain.Data

data SManagementReq r where
  StartSession :: UserId -> SManagementReq ActionKey
  Validate     :: UserId -> ActionKey -> SManagementReq Bool
  StopSession  :: UserId -> ActionKey -> SManagementReq ()
