{-# LANGUAGE GADTs #-}
module OpenBrain.Backend.PostgreSQLBackend.InformationBackend where

import OpenBrain.Backend hiding (processInformation)
import OpenBrain.Backend.PostgreSQLBackend.Common
import OpenBrain.Backend.PostgreSQLBackend.Sql.InformationBackend     as IBackend
import OpenBrain.Backend.PostgreSQLBackend.Sql.InformationBackend.Get as IBackendG
import OpenBrain.Common

processInformation :: PostgreSQLBackend -> IBackendReq r -> IO r
processInformation b (AddContentMedia ci c)             = useBackend b $ addContentMedia' ci c
processInformation b (AddParticipant iid uid)           = useBackend b $ addParticipant' iid uid
processInformation b (CreateCollection ci iids)         = useBackend b $ createCollection' ci iids
processInformation b (CreateDiscussion ci iids dl dt)   = useBackend b $ createDiscussion' ci iids dl dt
processInformation b GetInformationCount                = useBackend b getInformationCount'
processInformation b (GetInformation iid)               = useBackend b $ getInformation' iid
processInformation b (GetInformations l o)              = useBackend b $ getInformations' l o
processInformation b (GetInformationCountAfter ct)      = useBackend b $ getInformationCountAfter' ct
processInformation b (GetInformationsAfter ct l o)      = useBackend b $ getInformationsAfter' ct l o
processInformation b (GetInformationCountBy uid)        = useBackend b $ getInformationCountBy' uid
processInformation b (GetInformationBy uid l o)         = useBackend b $ getInformationBy' uid l o
processInformation b (GetInformationParentsCount iid)   = useBackend b $ getInformationParentsCount' iid
processInformation b (GetInformationParents iid l o)    = useBackend b $ getInformationParents' iid l o
processInformation b (GetProfiledUsers iid)             = useBackend b $ getProfiledUsers' iid
processInformation b GetNextDeadline                    = useBackend b getNextDeadline'
processInformation b (UpdateContentMedia uid iid t d c) = useBackend b $ updateContentMedia' uid iid t d c
processInformation b (UpdateCollection c iids)          = useBackend b $ updateCollection' c iids
processInformation b (SetParticipant c uid p)           = useBackend b $ setParticipant' c uid p
processInformation b (Vote iid uid)                     = useBackend b $ vote' iid uid
processInformation b (SetChoices iid iids)              = useBackend b $ setChoices' iid iids
processInformation b (DeleteInformation iid)            = useBackend b $ deleteInformation' iid
processInformation b (RemoveParticipant iid uid)        = useBackend b $ removeParticipant' iid uid
