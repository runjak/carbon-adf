{-# LANGUAGE Rank2Types #-}
module OpenBrain.Backend.PostgreSQLBackend.Common(
  PostgreSQLBackend(..)
, mkBackend, useBackend
-- | openBrain modules
, module Convertibles
, module Common
, module DId
-- | external modules
, module CMonad, module CMTrans
, module HDBC
)where

import Control.Monad as CMonad
import Control.Monad.Trans as CMTrans
import Database.HDBC as HDBC

import OpenBrain.Config (Config)
import OpenBrain.Common  as Common
import OpenBrain.Data.Id as DId
import qualified OpenBrain.Backend.PostgreSQLBackend.Sql.Convertibles as Convertibles

data PostgreSQLBackend = PostgreSQLBackend {
    config  :: Config
  , conn    :: HDBC.ConnWrapper  
  }

mkBackend :: IConnection conn => Config -> conn -> PostgreSQLBackend
mkBackend config = PostgreSQLBackend config . HDBC.ConnWrapper

useBackend :: PostgreSQLBackend -> (forall conn . IConnection conn => conn -> IO a) -> IO a
useBackend b f = withWConn (conn b) $ flip withTransaction f

