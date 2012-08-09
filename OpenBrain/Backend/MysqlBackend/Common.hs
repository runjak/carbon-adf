module OpenBrain.Backend.MysqlBackend.Common where

import qualified Database.HDBC as HDBC

import OpenBrain.Config (Config)

data MysqlBackend = MysqlBackend {
    config  :: Config
  , conn    :: HDBC.ConnWrapper  
  }

mkBackend :: (HDBC.IConnection conn) => Config -> conn -> MysqlBackend
mkBackend config = MysqlBackend config . HDBC.ConnWrapper
