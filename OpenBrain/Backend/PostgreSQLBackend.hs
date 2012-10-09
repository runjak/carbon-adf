module OpenBrain.Backend.PostgreSQLBackend (load, validConfig) where

import Control.Monad
import Data.Maybe
import qualified Database.HDBC as HDBC
import Database.HDBC.PostgreSQL as PSQL

import OpenBrain.Backend as B
import OpenBrain.Backend.PostgreSQLBackend.InformationBackend
import OpenBrain.Backend.PostgreSQLBackend.KarmaBackend
import OpenBrain.Backend.PostgreSQLBackend.RelationBackend
import OpenBrain.Backend.PostgreSQLBackend.SaltShaker
import OpenBrain.Backend.PostgreSQLBackend.SessionManagement
import OpenBrain.Backend.PostgreSQLBackend.UserBackend
import OpenBrain.Config as C
import qualified OpenBrain.Backend.PostgreSQLBackend.Common as Common

load :: Config -> IO Backend
load config = do
  unless (validConfig config) $ error "Invalid config for PostgreSqlBackend!"
  let options = pgOptions $ backendType config
  putStr "Connecting to PostgreSQL database:\t"
  conn <- PSQL.connectPostgreSQL options
  putStrLn "[ OK ]"
  return . Backend $ Common.mkBackend config conn

validConfig :: Config -> Bool
validConfig c = case backendType c of
                  (PostgreSQLBackend _) -> True
                  _ -> False

instance GeneralBackend Common.PostgreSQLBackend where
  shutdown            = shutdown'

-- | Used in Backend instance for Common.MysqlBackend
shutdown' :: Common.PostgreSQLBackend -> IO ()
shutdown' b = do
  let conn = Common.conn b
  putStr "Disconnecting from MySQL…\t"
  HDBC.commit conn >> HDBC.disconnect conn
  putStrLn "[ OK ]"
