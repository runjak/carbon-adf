module OpenBrain.Backend.Load (loadBackend) where
{-
  Backend.Load provides the loading procedure to build a Backend from the Config.
-}
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import OpenBrain.Backend
import OpenBrain.Config
import OpenBrain.Common
import qualified OpenBrain.Backend.PostgreSQLBackend as Psql

loadBackend :: Config -> MaybeT IO CBackend
loadBackend c
  | Psql.validConfig c = liftIO $ Psql.load c
  | otherwise = mzero
