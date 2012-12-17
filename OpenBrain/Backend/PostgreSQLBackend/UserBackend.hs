module OpenBrain.Backend.PostgreSQLBackend.UserBackend (getUser', getNobody') where

import Data.Maybe
import System.Time as T

import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.PostgreSQLBackend.Sql.UserBackend

instance UserBackend PostgreSQLBackend where
  login uname hash b         = useBackend b $ login' uname hash
  getUser uid b              = useBackend b $ getUser' uid
  getNobody b                = useBackend b getNobody'
  hasUserWithId uid b        = useBackend b $ hasUserWithId' uid
  hasUserWithName uname b    = useBackend b $ hasUserWithName' uname
  register uname hash salt b = useBackend b $ register' uname hash salt
  delete uid heir b          = useBackend b $ delete' uid heir
  getUserCount b             = useBackend b getUserCount'
  getUserList l o b          = useBackend b $ getUserList' l o
  updateKarma uid f b        = useBackend b $ updateKarma' uid f
  updatePasswd uid hash b    = useBackend b $ updatePasswd' uid hash
  setAdmin uid state b       = useBackend b $ setAdmin' uid state
  setProfile uid miid b      = useBackend b $ setProfile' uid miid

