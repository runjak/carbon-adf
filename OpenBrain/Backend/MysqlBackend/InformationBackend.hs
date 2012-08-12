module OpenBrain.Backend.MysqlBackend.InformationBackend () where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Database.HDBC as H
import System.Time (CalendarTime)

import OpenBrain.Data.Id
import OpenBrain.Data.Information
import OpenBrain.Backend
import OpenBrain.Backend.MysqlBackend.Convertibles ()
import OpenBrain.Backend.MysqlBackend.Common
import OpenBrain.Backend.Types as Types

instance InformationBackend MysqlBackend where
  addContentMedia             b = withWConn (conn b) addContentMedia'
  addToCollection             b = withWConn (conn b) addToCollection'
  addParticipant              b = withWConn (conn b) addParticipant'
  createCollection            b = withWConn (conn b) createCollection'
  createDiscussion            b = withWConn (conn b) createDiscussion'
  getInformationCount         b = withWConn (conn b) getInformationCount'
  getInformation              b = withWConn (conn b) getInformation'
  getInformations             b = withWConn (conn b) getInformations'
  getInformationsAfter        b = withWConn (conn b) getInformationsAfter'
  getInformationCountBy       b = withWConn (conn b) getInformationCountBy'
  getInformationBy            b = withWConn (conn b) getInformationBy'
  getInformationParentsCount  b = withWConn (conn b) getInformationParentsCount'
  getInformationParents       b = withWConn (conn b) getInformationParents'
  updateDescription           b = withWConn (conn b) updateDescription'
  updateTitle                 b = withWConn (conn b) updateTitle'
  updateContent               b = withWConn (conn b) updateContent'
  vote                        b = withWConn (conn b) vote'
  deleteInformation           b = withWConn (conn b) deleteInformation'
  removeParticipant           b = withWConn (conn b) removeParticipant'

addContentMedia' :: (IConnection conn) => conn -> Types.CreateInformation -> Types.Content -> IO InformationId
addContentMedia' = undefined

addToCollection' :: (IConnection conn) => conn -> Types.Target -> InformationId -> IO InformationId
addToCollection' = undefined

addParticipant' :: (IConnection conn) => conn -> InformationId -> UserId -> IO ()
addParticipant' = undefined

createCollection' :: (IConnection conn) => conn -> [InformationId] -> IO InformationId
createCollection' = undefined

createDiscussion' :: (IConnection conn) => conn -> [InformationId] -> Types.Deadline -> Types.DiscussionType -> IO InformationId
createDiscussion' = undefined

getInformationCount' :: (IConnection conn) => conn -> IO Types.Count
getInformationCount' = undefined

getInformation' :: (IConnection conn) => conn -> InformationId -> MaybeT IO Information
getInformation' = undefined

getInformations' :: (IConnection conn) => conn -> Types.Limit -> Types.Offset -> IO [Information]
getInformations' = undefined

getInformationsAfter' :: (IConnection conn) => conn -> Types.Limit -> CalendarTime -> IO [Information]
getInformationsAfter' = undefined

getInformationCountBy' :: (IConnection conn) => conn -> UserId -> IO Types.Count
getInformationCountBy' = undefined

getInformationBy' :: (IConnection conn) => conn -> UserId -> Types.Limit -> Types.Offset -> IO [Information]
getInformationBy' = undefined

getInformationParentsCount' :: (IConnection conn) => conn -> InformationId -> IO Types.Count
getInformationParentsCount' = undefined

getInformationParents' :: (IConnection conn) => conn -> Types.Limit -> Types.Offset -> IO [Information]
getInformationParents' = undefined

updateDescription' :: (IConnection conn) => conn -> InformationId -> Types.Description -> IO InformationId
updateDescription' = undefined

updateTitle' :: (IConnection conn) => conn -> InformationId -> Types.Title -> IO InformationId
updateTitle' = undefined

updateContent' :: (IConnection conn) => conn -> InformationId -> Types.Content -> IO InformationId
updateContent' = undefined

vote' :: (IConnection conn) => conn -> InformationId -> UserId -> IO ()
vote' = undefined

deleteInformation' :: (IConnection conn) => conn -> InformationId -> IO ()
deleteInformation' = undefined

removeParticipant' :: (IConnection conn) => conn -> InformationId -> UserId -> IO ()
removeParticipant' = undefined

