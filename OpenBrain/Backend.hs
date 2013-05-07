module OpenBrain.Backend (loadBackend) where
{-
  Provides the loading procedure to build a Backend from the Config.
-}
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import OpenBrain.Backend.DSL 
import OpenBrain.Config
import OpenBrain.Common
import qualified OpenBrain.Backend.PostgreSQL as Psql

loadBackend :: Config -> MaybeT IO CBackendProcessor
loadBackend c
  | Psql.validConfig c = liftIO $ Psql.load c
  | otherwise          = mzero
