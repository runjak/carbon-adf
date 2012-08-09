module OpenBrain.Backend.MysqlBackend (load, validConfig) where

import Control.Monad
import Data.Maybe
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.MySQL as MySQL

import OpenBrain.Backend as B
import OpenBrain.Backend.MysqlBackend.KarmaBackend
import OpenBrain.Backend.MysqlBackend.SaltShaker
import OpenBrain.Backend.MysqlBackend.SessionManagement
import OpenBrain.Backend.MysqlBackend.UserBackend
import OpenBrain.Config as C
import qualified OpenBrain.Backend.MysqlBackend.Common as Common

load :: Config -> IO CBackend
load config = do
  when (not $ validConfig config) $ error "Invalid config for MysqlBackend!"
  let mb    = backendType config
      cInfo = MySQL.MySQLConnectInfo (mysqlHost mb) (mysqlUser mb) (mysqlPassword mb) (mysqlDatabase mb) (mysqlPort mb) "" Nothing
  putStr "Connecting to MySQL…\t"
  conn <- MySQL.connectMySQL cInfo
  putStrLn "[ OK ]"
  when (isJust $ mysqlSchemaUpdate mb) $ do
    let fp = fromJust $ mysqlSchemaUpdate mb
    putStr $ "Updating db with file: '" ++ fp ++ "'…\t"
    readFile fp >>= HDBC.runRaw conn
    putStrLn "[ OK ]"
  return . CBackend $ Common.mkBackend config conn

validConfig :: Config -> Bool
validConfig = isMysqlBackend . backendType
  where
    isMysqlBackend (MysqlBackend _ _ _ _ _ _) = True
    isMysqlBackend _ = False

instance Backend Common.MysqlBackend where
  informationBackend  = undefined
  relationBackend     = undefined
  shutdown            = shutdown'
  userBackend         = CUserBackend
  karmaBackend        = CKarmaBackend
  saltShaker          = CSaltShaker
  sessionManagement   = CSessionManagement

-- | Used in Backend instance for Common.MysqlBackend
shutdown' :: Common.MysqlBackend -> IO ()
shutdown' b = do
  let conn = Common.conn b
  putStr "Disconnecting from MySQL…\t"
  HDBC.commit conn >> HDBC.disconnect conn
  putStrLn "[ OK ]"
