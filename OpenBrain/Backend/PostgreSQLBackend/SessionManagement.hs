{-# LANGUAGE GADTs #-}
module OpenBrain.Backend.PostgreSQLBackend.SessionManagement where

import System.Random

import OpenBrain.Backend hiding (processSession)
import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.PostgreSQLBackend.Sql.SessionManagement

processSession :: PostgreSQLBackend -> SManagementReq r -> IO r
processSession b (StartSession uid)   = useBackend b $ startSession' uid
processSession b (Validate uid ak)    = useBackend b $ validate' uid ak
processSession b (StopSession uid ak) = useBackend b $ stopSession' uid ak
