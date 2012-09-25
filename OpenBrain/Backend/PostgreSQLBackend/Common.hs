module OpenBrain.Backend.PostgreSQLBackend.Common where

import qualified Database.HDBC as HDBC

import OpenBrain.Config (Config)

data PostgreSQLBackend = PostgreSQLBackend {
    config  :: Config
  , conn    :: HDBC.ConnWrapper  
  }

mkBackend :: (HDBC.IConnection conn) => Config -> conn -> PostgreSQLBackend
mkBackend config = PostgreSQLBackend config . HDBC.ConnWrapper
