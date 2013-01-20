{-# LANGUAGE GADTs #-}
module OpenBrain.Backend.PostgreSQLBackend.SaltShaker where

import OpenBrain.Backend hiding (processSalt)
import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.PostgreSQLBackend.Sql.SaltShaker

processSalt :: PostgreSQLBackend -> SaltShakerReq r -> IO r
processSalt b (GetSalt uid) = useBackend b $ getSalt' uid
