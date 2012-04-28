module OpenBrain.Backend.Load (loadBackend) where
{-
  Backend.Load provides the loading procedure to build a Backend from the Config.
-}
import OpenBrain.Backend(ProxyBackend)
import qualified OpenBrain.Backend.RamBackend as RB
import OpenBrain.Config

import Control.Monad

loadBackend :: Config -> IO (Maybe ProxyBackend)
loadBackend = loadBackend' . backendType

loadBackend' :: BackendType -> IO (Maybe ProxyBackend)
loadBackend' MissingBackend = return Nothing
loadBackend' RamBackend     = liftM Just RB.load
