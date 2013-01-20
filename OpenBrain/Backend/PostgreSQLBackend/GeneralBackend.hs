{-# LANGUAGE GADTs #-}
module OpenBrain.Backend.PostgreSQLBackend.GeneralBackend where

import qualified Database.HDBC as HDBC

import OpenBrain.Backend hiding (processGeneral)
import OpenBrain.Backend.PostgreSQLBackend.Common as Common

processGeneral :: PostgreSQLBackend -> GBackendReq r -> IO r
processGeneral b Shutdown = shutdown' b
processGeneral _ (Nop r)  = return r

shutdown' :: Common.PostgreSQLBackend -> IO ()
shutdown' b = do
  let conn = Common.conn b
  putStr "Disconnecting from PostgreSQL…\t"
  HDBC.commit conn >> HDBC.disconnect conn
  putStrLn "[ OK ]"
