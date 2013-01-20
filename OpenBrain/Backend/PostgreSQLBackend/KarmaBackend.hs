{-# LANGUAGE GADTs #-}
module OpenBrain.Backend.PostgreSQLBackend.KarmaBackend where

import OpenBrain.Backend hiding (processKarma)
import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.PostgreSQLBackend.Sql.KarmaBackend
import OpenBrain.Config
import OpenBrain.Config.Karma

processKarma :: PostgreSQLBackend -> KBackendReq r -> IO r
processKarma b KarmaDeleteUser = liftM (test b ratioDeleteUser) $ useBackend b maxKarma
processKarma b KarmaEditUser   = liftM (test b ratioEditUser) $ useBackend b maxKarma
