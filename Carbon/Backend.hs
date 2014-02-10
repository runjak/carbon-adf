module Carbon.Backend (loadBackend, tryBackend) where
{-
  Provides the loading procedure to build a Backend from the Config.
-}
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Carbon.Backend.DSL as DSL
import Carbon.Config      as Config
import Carbon.Common      as Common
import qualified Carbon.Backend.PostgreSQL as Psql

loadBackend :: Config -> MaybeT IO CBackendProcessor
loadBackend c
  | Psql.validConfig c = liftIO $ Psql.load c
  | otherwise          = mzero

tryBackend :: FilePath -> IO (BackendDSL r -> IO r)
tryBackend path = do
  mconf <- Config.readConfig path
  case mconf of
    Nothing  -> error "Could not load Config."
    (Just c) -> do
      mback <- runMaybeT $ loadBackend c
      case mback of
        Nothing  -> error "Could not load Backend."
        (Just b) -> return $ process b
