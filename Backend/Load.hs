module Backend.Load (loadBackend) where
{-
  Backend.Load provides the loading procedure to build a Backend from the Config.
-}
import Backend(ProxyBackend)
import qualified Backend.RamBackend as RB
import Config

import Control.Monad

loadBackend :: Config -> IO (Maybe ProxyBackend)
loadBackend = loadBackend' . backendType

loadBackend' :: BackendType -> IO (Maybe ProxyBackend)
loadBackend' MissingBackend = return Nothing
loadBackend' RamBackend     = liftM Just RB.load
