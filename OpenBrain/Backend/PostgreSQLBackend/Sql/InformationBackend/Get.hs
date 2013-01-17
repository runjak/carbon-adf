module OpenBrain.Backend.PostgreSQLBackend.Sql.InformationBackend.Get where

import Data.Maybe
import System.Time

import OpenBrain.Backend.PostgreSQLBackend.Common hiding (clone)
import OpenBrain.Backend.PostgreSQLBackend.Sql.UserBackend (getUser', getNobody')
import OpenBrain.Data

getInformationCount' :: (IConnection conn) => conn -> IO Count
getInformationCount' conn = do
  [[c]] <- quickQuery' conn "SELECT COUNT(*) FROM \"Information\" WHERE deletion IS NULL" []  
  return $ fromSql c

getInformation' :: (IConnection conn) => InformationId -> conn -> IO (Maybe Information)
getInformation' iid conn = do
  let selectInformation = "SELECT author, creation, deletion, description, title, mediaid "
                       ++ "FROM \"Information\" WHERE informationid = ?"
  rst <- liftIO $ quickQuery' conn selectInformation [toSql $ toId iid]
  case rst of
    [[_author, _creation, _deletion, _description, _title, mediaid]] -> do
      mUser  <- flip getUser' conn . fromId $ fromSql _author
      case mUser of
        Nothing -> return Nothing
        (Just user) -> do
          media <- liftIO $ getMedia iid conn
          return $ Just Information{
            author              = user
          , informationCreation = fromSql _creation
          , informationDeletion = fromSql _deletion
          , iDescription         = fromSql _description
          , informationId       = iid
          , media               = media
          , iTitle               = fromSql _title
          }
    _ -> return Nothing

getMedia :: (IConnection conn) => InformationId -> conn -> IO Media
getMedia iid conn = do
  let selectMedia = "SELECT M.mediaid, M.content, M.collectiontype, M.discussionid "
                 ++ "FROM \"Media\" AS M JOIN \"Information\" AS I USING (mediaid) "
                 ++ "WHERE I.informationid = ?"
      iid'        = toSql $ toId iid
  [[mid, _content, _ctype, did]] <- quickQuery' conn selectMedia [iid']
  -- Two constructors for Media:
  if _content /= SqlNull
    then return . Content $ fromSql _content
    else do -- Media is a Collection:
      let ctype = fromSql _ctype :: CollectionType
      -- Fetching arguments:
      let argQuery = "SELECT target FROM \"Relations\" WHERE source = ? AND type = ?"
      _args <- quickQuery' conn argQuery [iid', toSql Collection]
      let args = map (fromId . fromSql . head) _args
      -- Looking for DiscussionInfo:
      dinfo <- getDiscussionInfo did conn
      -- Building the complete Collection:
      return ICollection {
        arguments       = args
      , collectionType  = ctype
      , discussion      = dinfo
      }

type DiscussionId = SqlValue
getDiscussionInfo :: (IConnection conn) => DiscussionId -> conn -> IO (Maybe DiscussionInfo)
getDiscussionInfo did conn = do
  guard $ did /= SqlNull
  -- Fetching basic discussion info:
  let q = "SELECT complete, deadline FROM \"DiscussionInfo\" WHERE discussionid = ?"
  [[_complete, _deadline]] <- liftIO $ quickQuery' conn q [did]
  -- Fetching complete:
  let mkComplete = flip getInformation' conn . fromId $ fromSql _complete
  complete <- (_complete /= SqlNull) ? (mkComplete, return Nothing)
  -- Fetching the choices:
  let q = "SELECT informationid, votes FROM \"DiscussionChoices\" WHERE discussionid = ?"
  _choices  <- liftIO $ quickQuery' conn q [did]
  choices   <- liftM catMaybes $ mapM (mkChoice conn) _choices
  -- Fetching the participants:
  let q = "SELECT userid, voted FROM \"DiscussionParticipants\" WHERE discussionid = ?"
  _parts  <- liftIO $ quickQuery' conn q [did]
  parts   <- liftM catMaybes $ mapM (mkPart conn) _parts
  -- Building the complete DiscussionInfo:
  return $ Just DiscussionInfo {
    choices       = choices
  , complete      = complete
  , deadline      = fromSql _deadline
  , participants  = parts
  }
  where
    mkChoice :: (IConnection conn) => conn -> [SqlValue] -> IO (Maybe (Information, Votes))
    mkChoice conn [_iid, v] = do
      mInf <- flip getInformation' conn . fromId $ fromSql _iid
      maybe (return Nothing) (\i -> return $ Just (i, fromSql v)) mInf
    mkChoice _ _            = mzero
    
    mkPart :: (IConnection conn) => conn -> [SqlValue] -> IO (Maybe (UserData, Voted))
    mkPart conn [_uid, v] = do
      mUdata <- flip getUser' conn . fromId $ fromSql _uid
      maybe (return Nothing) (\u -> return $ Just (u, fromSql v)) mUdata
    mkPart _ _            = mzero

getInformations' :: (IConnection conn) => Limit -> Offset -> conn -> IO [Information]
getInformations' limit offset conn = do
  let q = "SELECT informationid FROM \"Information\" WHERE deletion IS NULL ORDER BY creation DESC LIMIT ? OFFSET ?"
  rst <- quickQuery' conn q [toSql limit, toSql offset]
  liftM catMaybes $ mapM (flip getInformation' conn . fromId . fromSql . head) rst

getInformationCountAfter' :: (IConnection conn) => CalendarTime -> conn -> IO Count
getInformationCountAfter' ctime conn = do
  let q = "SELECT COUNT(*) FROM \"Information\" WHERE deletion IS NULL AND creation > ?"
  [[c]] <- quickQuery' conn q [toSql ctime]
  return $ fromSql c

getInformationsAfter' :: (IConnection conn) => CalendarTime -> Limit -> Offset -> conn -> IO [Information]
getInformationsAfter' ctime limit offset conn = do
  let q = "SELECT informationid FROM \"Information\" WHERE deletion IS NULL AND creation > ? ORDER BY creation DESC LIMIT ? OFFSET ?"
  rst <- quickQuery' conn q [toSql ctime, toSql limit, toSql offset]
  liftM catMaybes $ mapM (flip getInformation' conn . fromId . fromSql . head) rst

getInformationCountBy' :: (IConnection conn) => UserId -> conn -> IO Count
getInformationCountBy' uid conn = do
  let q = "SELECT COUNT(*) FROM \"Information\" WHERE author = ? AND deletion IS NULL"
  [[c]] <- quickQuery' conn q [toSql $ toId uid]
  return $ fromSql c

getInformationBy' :: (IConnection conn) => UserId -> Limit -> Offset -> conn -> IO [Information]
getInformationBy' uid limit offset conn = do
  let q = "SELECT informationid FROM \"Information\" WHERE author = ? AND deletion IS NULL ORDER BY creation DESC LIMIT ? OFFSET ?"
  rst <- quickQuery' conn q [toSql $ toId uid, toSql limit, toSql offset]
  liftM catMaybes $ mapM (flip getInformation' conn . fromId . fromSql . head) rst

getInformationParentsCount' :: (IConnection conn) => InformationId -> conn -> IO Count
getInformationParentsCount' iid conn = do
  let q = "SELECT COUNT(*) FROM \"Relations\" WHERE type = ? AND target = ?"
  [[c]] <- quickQuery' conn q [toSql Parent, toSql $ toId iid]
  return $ fromSql c

getInformationParents' :: (IConnection conn) => InformationId -> Limit -> Offset -> conn -> IO [Information]
getInformationParents' iid limit offset conn = do
  let q = "SELECT source FROM \"Relations\" WHERE target = ? ORDER BY creation DESC LIMIT ? OFFSET ?"
  rst <- quickQuery' conn q [toSql $ toId iid, toSql limit, toSql offset]
  liftM catMaybes $ mapM (flip getInformation' conn . fromId . fromSql . head) rst

getProfiledUsers' :: (IConnection conn) => InformationId -> conn -> IO [UserData]
getProfiledUsers' iid conn = do
  _uids <- quickQuery' conn "SELECT userid FROM \"UserData\" WHERE profile = ?" [toSql $ toId iid]
  muds  <- mapM (flip getUser' conn . fromId . fromSql . head) _uids
  return $ catMaybes muds

getNextDeadline' :: (IConnection conn) => conn -> IO (Maybe Information)
getNextDeadline' conn = do
  let q = "SELECT informationid FROM \"Information\" "
        ++"JOIN \"Media\" USING (mediaid) "
        ++"JOIN \"DiscussionInfo\" USING (discussionid) "
        ++"WHERE complete IS NULL ORDER BY deadline ASC LIMIT 1";
  rst <- liftM (listToMaybe . map (fromId . fromSql . head)) $ quickQuery' conn q []
  case rst of
    Nothing    -> return Nothing
    (Just iid) -> getInformation' iid conn

