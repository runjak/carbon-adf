module OpenBrain.Backend.PostgreSQLBackend.SaltShaker () where

import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.PostgreSQLBackend.Sql.SaltShaker
import OpenBrain.Data.Id
import OpenBrain.Data.Salt

instance SaltShaker PostgreSQLBackend where
  getSalt uid b = useBackend b $ getSalt' uid
