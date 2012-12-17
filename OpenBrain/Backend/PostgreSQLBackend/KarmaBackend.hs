module OpenBrain.Backend.PostgreSQLBackend.KarmaBackend () where

import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.PostgreSQLBackend.Sql.KarmaBackend
import OpenBrain.Config
import OpenBrain.Config.Karma
import OpenBrain.Data.Karma

instance KarmaBackend PostgreSQLBackend where
  karmaDeleteUser b = liftM (test b ratioDeleteUser) $ useBackend b maxKarma
  karmaEditUser   b = liftM (test b ratioEditUser) $ useBackend b maxKarma

