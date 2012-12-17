{-# LANGUAGE ScopedTypeVariables #-}
module OpenBrain.Backend.PostgreSQLBackend.SessionManagement () where

import System.Random

import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.PostgreSQLBackend.Sql.SessionManagement
import OpenBrain.Backend.Types
import OpenBrain.Data.Id

instance SessionManagement PostgreSQLBackend where
  startSession uid b   = useBackend b $ startSession' uid
  validate uid ak b    = useBackend b $ validate' uid ak
  stopSession uid ak b = useBackend b $ stopSession' uid ak

