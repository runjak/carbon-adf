{-# LANGUAGE DoAndIfThenElse #-}
module OpenBrain.Backend.PostgreSQLBackend.InformationBackend () where

import Data.Maybe

import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.PostgreSQLBackend.Sql.InformationBackend
import OpenBrain.Backend.PostgreSQLBackend.Sql.InformationBackend.Get
import OpenBrain.Common
import OpenBrain.Data

instance InformationBackend PostgreSQLBackend where
  addContentMedia ci c b             = useBackend b $ addContentMedia' ci c
  addParticipant iid uid b           = useBackend b $ addParticipant' iid uid
  createCollection ci iids b         = useBackend b $ createCollection' ci iids
  createDiscussion ci iids dl dt b   = useBackend b $ createDiscussion' ci iids dl dt
  getInformationCount b              = useBackend b getInformationCount'
  getInformation iid b               = useBackend b $ getInformation' iid
  getInformations l o b              = useBackend b $ getInformations' l o
  getInformationCountAfter t b       = useBackend b $ getInformationCountAfter' t
  getInformationsAfter t l o b       = useBackend b $ getInformationsAfter' t l o
  getInformationCountBy uid b        = useBackend b $ getInformationCountBy' uid
  getInformationBy uid l o b         = useBackend b $ getInformationBy' uid l o
  getInformationParentsCount iid b   = useBackend b $ getInformationParentsCount' iid
  getInformationParents iid l o b    = useBackend b $ getInformationParents' iid l o
  getProfiledUsers iid b             = useBackend b $ getProfiledUsers' iid
  getNextDeadline b                  = useBackend b getNextDeadline'
  updateContentMedia uid iid t d c b = useBackend b $ updateContentMedia' uid iid t d c
  updateCollection c iids b          = useBackend b $ updateCollection' c iids
  setParticipant c uid p b           = useBackend b $ setParticipant' c uid p
  vote iid uid b                     = useBackend b $ vote' iid uid
  setChoices iid iids b              = useBackend b $ setChoices' iid iids
  deleteInformation iid b            = useBackend b $ deleteInformation' iid
  removeParticipant iid uid b        = useBackend b $ removeParticipant' iid uid

