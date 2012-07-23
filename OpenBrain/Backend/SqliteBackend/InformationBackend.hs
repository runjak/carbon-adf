module OpenBrain.Backend.SqliteBackend.InformationBackend () where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Database.HDBC as H
import System.Time as T

import OpenBrain.Backend
import OpenBrain.Backend.SqliteBackend.Convertibles
import OpenBrain.Backend.SqliteBackend.Common
import OpenBrain.Data.Information
import OpenBrain.Data.User (UserIdentifier)
import qualified OpenBrain.Data.User as User

{-
class InformationBackend b where
  addInformation        :: (UserIdentifier ui) b => b -> ui -> Title -> Description -> Media -> IO ()
  deleteInformation     :: (InformationIdentifier i, UserIdentifier ui) b => b -> ui -> i -> IO ()
  getInformationCount   :: b -> IO Int
  getInformation        :: (InformationIdentifier i) b => b -> i -> MaybeT IO Information
  getInformations       :: b -> Limit -> Offset -> IO [Information]                 -- | No parents
  getInformationAfter   :: b -> Limit -> CalendarTime -> IO [Information]           -- | No parents
  getInformationBy      :: (UserIdentifier ui) b => b -> ui -> IO [Information]       -- | No parents
  getInformationParents :: (InformationIdentifier i) b => b -> i -> IO [Information]  -- | youngest first
  updateDescription     :: (InformationIdentifier i, UserIdentifier ui) b => b -> ui -> i -> Description -> IO ()
  updateMedia           :: (InformationIdentifier i, UserIdentifier ui) b => b -> ui -> i -> Media -> IO ()
  updateTitle           :: (InformationIdentifier i, UserIdentifier ui) b => b -> ui -> i -> Title -> IO ()
-}

instance InformationBackend SqliteBackend where
  addInformation        b = withWConn (conn b) addInformation'
  deleteInformation     b = withWConn (conn b) deleteInformation'
  getInformationCount   b = withWConn (conn b) getInformationCount'
  getInformation        b = withWConn (conn b) getInformation'
  getInformations       b = withWConn (conn b) getInformations'
  getInformationAfter   b = withWConn (conn b) getInformationAfter'
  getInformationBy      b = withWConn (conn b) getInformationBy'
  getInformationParents b = withWConn (conn b) getInformationParents'
  updateDescription     b = withWConn (conn b) updateDescription'
  updateMedia           b = withWConn (conn b) updateMedia'
  updateTitle           b = withWConn (conn b) updateTitle'

addInformation' :: (UserIdentifier ui, IConnection conn) => conn -> ui -> Title -> Description -> Media -> IO ()
addInformation' conn ui title description media = do
  let author = toSql $ User.getUserId ui
  creation      <- liftM (toSql . toUTCTime) getClockTime
  stmt          <- prepare conn "INSERT INTO Information(author, creation, description, title) VALUES (?, ?, ?, ?)"
  execute stmt [author, creation, toSql description, toSql title]
  informationid <- lastInsertRowId conn
  insertMedia conn informationid media

deleteInformation' :: (InformationIdentifier i, IConnection conn) => conn -> i -> IO ()
deleteInformation' conn i = undefined

getInformationCount' :: (IConnection conn) => conn -> IO Int
getInformationCount' conn = undefined

getInformation' :: (InformationIdentifier i, IConnection conn) => conn -> i -> MaybeT IO Information
getInformation' conn i = undefined

getInformations' :: (IConnection conn) => conn -> Limit -> Offset -> IO [Information]
getInformations' conn limit offset = undefined

getInformationAfter' :: (IConnection conn) => conn -> Limit -> CalendarTime -> IO [Information]
getInformationAfter' conn limit time = undefined

getInformationBy' :: (UserIdentifier ui, IConnection conn) => conn -> ui -> IO [Information]
getInformationBy' conn ui = undefined

getInformationParents' :: (InformationIdentifier i, IConnection conn) => conn -> i -> IO [Information]
getInformationParents' conn i = undefined

updateDescription' :: (InformationIdentifier i, UserIdentifier ui, IConnection conn) => conn -> ui -> i -> Description -> IO ()
updateDescription' conn ui i description = undefined

updateMedia' :: (InformationIdentifier i, UserIdentifier ui, IConnection conn) => conn -> ui -> i -> Media -> IO ()
updateMedia' conn ui i media = undefined

updateTitle' :: (InformationIdentifier i, UserIdentifier ui, IConnection conn) => conn -> ui -> i -> Title -> IO ()
updateTitle' conn ui i title = undefined

{-
  Integer must be the InformationId that the media belongs to.
-}
insertMedia :: (IConnection conn) => conn -> Integer -> Media -> IO ()
insertMedia conn informationid media = case media of
  (Media text) -> do
    insertContent <- prepare conn "INSERT INTO MediaContent(content) VALUES (?)"
    execute insertContent [toSql text]
    mediacontentid <- lastInsertRowId conn
    stmt <- prepare conn "UPDATE Information SET mediacontentid = ? WHERE informationid = ?"
    execute stmt [toSql mediacontentid, toSql informationid] >> commit conn
  (Collection targets) -> insertMediaBundle conn False informationid $ map (toSql . getInformationId) targets
  (Decision targets) -> insertMediaBundle conn True informationid $ map (toSql . getInformationId) targets
  (Discussion arguments afType choices complete deadline participants) -> do
    insertMediaDiscussion <- prepare conn "INSERT INTO MediaDiscussion(aftype, complete, deadline) VALUES (?, ?, ?)"
    execute insertMediaDiscussion [toSql afType, toSql $ liftM getInformationId complete, toSql deadline]
    discussionid <- lastInsertRowId conn
    -- Insert arguments
    -- Insert choices
    -- Insert participants
    undefined

insertMediaBundle :: (IConnection conn) => conn -> Bool -> Integer -> [SqlValue] -> IO ()
insertMediaBundle conn isDecision informationid targets = do
  stmt <- prepare conn "INSERT INTO MediaBundle(informationid, targetid, isdecision) VALUES (?, ?, ?)"
  executeMany stmt $ forM targets $ \t -> [toSql informationid, t, toSql isDecision]
  commit conn
