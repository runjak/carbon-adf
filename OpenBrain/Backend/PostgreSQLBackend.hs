module OpenBrain.Backend.PostgreSQLBackend (load, validConfig) where

import Control.Monad
import Data.Maybe
import Database.HDBC.PostgreSQL as PSQL
import qualified Database.HDBC  as HDBC

import OpenBrain.Backend as B
import OpenBrain.Config  as C
import qualified OpenBrain.Backend.PostgreSQLBackend.Common             as Common
import qualified OpenBrain.Backend.PostgreSQLBackend.GeneralBackend     as GBackend
import qualified OpenBrain.Backend.PostgreSQLBackend.InformationBackend as IBackend
import qualified OpenBrain.Backend.PostgreSQLBackend.KarmaBackend       as KBackend
import qualified OpenBrain.Backend.PostgreSQLBackend.RelationBackend    as RBackend
import qualified OpenBrain.Backend.PostgreSQLBackend.SaltShaker         as SShaker
import qualified OpenBrain.Backend.PostgreSQLBackend.SessionManagement  as SManagement
import qualified OpenBrain.Backend.PostgreSQLBackend.UserBackend        as UBackend

load :: Config -> IO CBackend
load config = do
  unless (validConfig config) $ error "Invalid config for PostgreSqlBackend!"
  let options = pgOptions $ backendType config
  putStr "Connecting to PostgreSQL database:\t"
  conn <- PSQL.connectPostgreSQL options
  putStrLn "[ OK ]"
  return . CBackend $ Common.mkBackend config conn

validConfig :: Config -> Bool
validConfig c = case backendType c of
                  (PostgreSQLBackend _) -> True
                  _ -> False

instance Backend Common.PostgreSQLBackend where
  processGeneral     b p = GBackend.processGeneral b p
  processInformation b p = IBackend.processInformation b p
  processKarma       b p = KBackend.processKarma b p
  processRelation    b p = RBackend.processRelation b p
  processSalt        b p = SShaker.processSalt b p
  processSession     b p = SManagement.processSession b p
  processUser        b p = UBackend.processUser b p
