module OpenBrain.Backend.PostgreSQLBackend.Sql.KarmaBackend where

import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Config
import OpenBrain.Config.Karma
import OpenBrain.Data.Karma

-- Finding the highest current value of Karma in the system:
maxKarma :: IConnection conn => conn -> IO Karma
maxKarma conn = do
  rst <- quickQuery' conn "SELECT MAX(karma) FROM \"UserData\"" []
  case rst of
    [[k]] -> return $ fromSql k
    _     -> return $ toKarma 0

test :: PostgreSQLBackend -> (KarmaConfig -> Rational) -> Highest -> Karma
test b f k = do
  let ratio = f . karmaConfig $ config b
  satisfiesRatio ratio k

