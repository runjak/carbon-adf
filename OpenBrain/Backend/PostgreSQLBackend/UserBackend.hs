{-# LANGUAGE GADTs #-}
module OpenBrain.Backend.PostgreSQLBackend.UserBackend where

import Data.Maybe
import System.Time as T

import OpenBrain.Backend hiding (delete')
import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.PostgreSQLBackend.Sql.UserBackend

processUser :: PostgreSQLBackend -> UBackendReq r -> IO r
processUser b (Login uname hash)          = useBackend b $ login' uname hash
processUser b (GetUser uid)               = useBackend b $ getUser' uid
processUser b GetNobody                   = useBackend b getNobody'
processUser b (HasUserWithId uid)         = useBackend b $ hasUserWithId' uid
processUser b (HasUserWithName uname)     = useBackend b $ hasUserWithName' uname
processUser b (Register uname hash salt)  = useBackend b $ register' uname hash salt
processUser b (Delete uid heir)           = useBackend b $ delete' uid heir
processUser b GetUserCount                = useBackend b getUserCount'
processUser b (GetUserList l o)           = useBackend b $ getUserList' l o
processUser b (UpdateKarma uid f)         = useBackend b $ updateKarma' uid f
processUser b (UpdatePasswd uid hash)     = useBackend b $ updatePasswd' uid hash
processUser b (SetAdmin uid t)            = useBackend b $ setAdmin' uid t
processUser b (SetProfile uid miid)       = useBackend b $ setProfile' uid miid
