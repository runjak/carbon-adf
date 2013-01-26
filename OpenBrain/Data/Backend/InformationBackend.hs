{-# LANGUAGE GADTs #-}
module OpenBrain.Data.Backend.InformationBackend where

import OpenBrain.Data

data IBackendReq r where
  -- | 'Creative' Operations:
  AddContentMedia  :: CreateInformation -> Content -> IBackendReq InformationId
  AddParticipant   :: InformationId -> UserId -> IBackendReq () -- | May only target discussions
  CreateCollection :: CreateInformation -> [InformationId] -> IBackendReq Collection
  CreateDiscussion :: CreateInformation -> [InformationId] -> Deadline -> DiscussionType -> IBackendReq InformationId
  -- | 'Querying' Operations:
  GetInformationCount        :: IBackendReq Count
  GetInformation             :: InformationId -> IBackendReq (Maybe Information)
  GetInformations            :: Limit -> Offset -> IBackendReq [Information] -- | No parents
  GetInformationCountAfter   :: CalendarTime -> IBackendReq Count
  GetInformationsAfter       :: CalendarTime ->Limit -> Offset -> IBackendReq [Information] -- | No parents
  GetInformationCountBy      :: UserId -> IBackendReq Count
  GetInformationBy           :: UserId -> Limit -> Offset -> IBackendReq [Information] -- | No parents
  GetInformationParentsCount :: InformationId -> IBackendReq Count
  GetInformationParents      :: InformationId -> Limit -> Offset -> IBackendReq [Information] -- | youngest first
  GetProfiledUsers           :: InformationId -> IBackendReq [UserData]
  GetNextDeadline            :: IBackendReq (Maybe Information)
  -- | 'Modifying' Operations:
  UpdateContentMedia :: UserId -> InformationId -> Title -> Description -> Content -> IBackendReq InformationId
  UpdateCollection   :: Collection -> [InformationId] -> IBackendReq Collection -- | Changes the items of the collection to the given list.
  SetParticipant     :: Collection -> UserId -> Bool -> IBackendReq ()
  Vote               :: InformationId -> UserId -> IBackendReq () -- | May only target CollectionType Choice - Discussion is found because it's a parent.
  {-|
    Adds the given lists of InformationId as collections to the system.
    They are than added as choices to the discussion defined by the single InformationId.
    This operation fails if targeted Information is no discussion or already has choices.
  |-}
  SetChoices :: InformationId -> [[InformationId]] -> IBackendReq ()
  -- | 'Destructive' Operations:
  DeleteInformation :: InformationId -> IBackendReq () -- | Sets a delete date on an Information
  RemoveParticipant :: InformationId -> UserId -> IBackendReq () -- | May only target discussions, should not be possible when voted.
