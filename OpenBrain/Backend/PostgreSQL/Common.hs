{-# LANGUAGE RankNTypes #-}
module OpenBrain.Backend.PostgreSQL.Common(
  Query, Connector, Executor
, valid, load, connect, exec
-- | openBrain modules
, module Conversion
, module Common
, module Data
, module DSL
-- | external modules
, module CMonad, module CMTrans
, module HDBC
)where

import Control.Concurrent.MVar (MVar)
import Control.Monad       as CMonad
import Control.Monad.Trans as CMTrans
import Database.HDBC       as HDBC
import qualified Control.Concurrent.MVar  as MVar
import qualified Database.HDBC.PostgreSQL as PSQL

import OpenBrain.Backend.DSL as DSL
import OpenBrain.Config (Config)
import OpenBrain.Common as Common
import OpenBrain.Data   as Data
import qualified OpenBrain.Backend.PostgreSQL.Conversion as Conversion
import qualified OpenBrain.Config  as Config

type   Query   r = forall conn . IConnection conn => conn -> IO r
data   Connector = Connector {conn :: MVar ConnWrapper}
newtype Executor = E ConnWrapper

valid :: Config -> Bool
valid c = case Config.backendType c of
  (Config.PostgreSQLBackend _) -> True
  _ -> False

load :: Config -> IO Connector
load = mkConnector . Config.pgOptions . Config.backendType
  where
    mkConnector :: String -> IO Connector
    mkConnector cInfo = do
      putStr "Connecting to PostgreSQL database:\t"
      c <- PSQL.connectPostgreSQL cInfo
      putStrLn "[ OK ]"
      liftM Connector . MVar.newMVar $ ConnWrapper c

connect :: Connector -> IO Executor
connect = liftM E . flip MVar.withMVar HDBC.clone . conn

exec :: Executor -> Query r -> IO r
exec (E wConn) q = withWConn wConn $ flip HDBC.withTransaction q
