module OpenBrain.Backend.Load (loadBackend) where
{-
  Backend.Load provides the loading procedure to build a Backend from the Config.
-}
import OpenBrain.Backend
import qualified OpenBrain.Backend.RamBackend as Ram
import OpenBrain.Config

import Control.Monad

loadBackend :: Config -> IO (Maybe Backend)
loadBackend c = case backendType c of
  RamBackend -> liftM Just $ Ram.load c
  MissingBackend -> return Nothing
  
