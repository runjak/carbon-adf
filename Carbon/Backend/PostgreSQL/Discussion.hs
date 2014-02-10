{-# LANGUAGE RankNTypes #-}
module Carbon.Backend.PostgreSQL.Discussion where

import Data.Monoid (Monoid(..))
import Data.Set (Set)
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Carbon.Backend.PostgreSQL.Common
import Carbon.Backend.PostgreSQL.Conversion

evaluate :: ItemId -> Query EvaluationState
evaluate = fail "FIXME implement Carbon.Backend.PostgreSQL.Discussion:evaluate"

getDiscussion :: SqlValue -> Query (Maybe (Discussion (Item Id)))
getDiscussion SqlNull _ = return Nothing
getDiscussion did conn = do
  let getBasic = "SELECT deadline, evaluation FROM discussions WHERE discussionid = ?"
      getParticipants = "SELECT userid FROM discussion_participants WHERE discussionid = ?"
      getArguments = "SELECT itemid FROM discussion_arguments WHERE discussionid = ?"
      mkIdSet = liftM $ Set.fromList . map (fromSql . head)
  [[deadline, evaluation]] <- quickQuery' conn getBasic [did]
  ps <- mkIdSet $ quickQuery' conn getParticipants [did]
  args <- mkIdSet $ quickQuery' conn getArguments [did]
  return . Just $ Discussion {
    discussionId = fromSql did
  , arguments    = Right args
  , deadline     = fromSql deadline
  , participants = ps
  , evaluation   = fromSql evaluation
  }

type SetItem = Item Id -> Query (Either Error (Item Id))

setDiscussion :: IConnection conn => SetItem -> conn -> Item Id -> IO (Either Error (Item Id))
setDiscussion setItem conn i
  | Maybe.isNothing (discussion i) = do -- Set Discussion to Null
    let q = "UPDATE items SET discussionid = NULL WHERE itemid = ?"
    quickQuery' conn q [toSql $ itemId i]
    return $ Right i
  | noId . discussionId $ getD i = do -- Create Discussion
    -- Inserting into discussions table
    let d = getD i
        q = "INSERT INTO discussions (deadline, evaluation) VALUES (?, ?) RETURNING discussionid"
    [[did]] <- quickQuery' conn q [toSql (deadline d), toSql $ evaluation d]
    -- Setting the discussion to the item:
    let q = "UPDATE items SET discussionid = ? WHERE itemid = ?"
    quickQuery' conn q [did, toSql $ itemId i]
    -- Inserting participants
    insertParticipants conn d did
    -- Checking if we need to set further items
    -- Maybe we wanna check if propagate throws exceptions.
    ids <- propagate conn $ arguments d
    -- Inserting arguments
    insertArguments conn ids did
    -- Finished:
    return . Right $ i <+ d <+ (fromSql did :: Id)
  | otherwise = do -- Update Discussion
    let d = getD i
    d' <- liftM Maybe.fromJust $ getDiscussion (toSql $ discussionId d) conn
    let boring = return $ Right i
        changed = setDiscussion setItem conn $ i <+ d <+ (mempty :: Id)
    (d == d') ? (boring, changed)
  where
    getD = Maybe.fromJust . discussion

    insertParticipants :: IConnection conn => conn -> Discussion (Item Id) -> SqlValue -> IO ()
    insertParticipants conn d did = do
      let q = "INSERT INTO discussion_participants (discussionid, userid) VALUES (?, ?)"
      stmt <- prepare conn q
      executeMany stmt . map (\u -> [did, toSql u]) . Set.toList $ participants d

    propagate :: IConnection conn => conn -> Either (Set.Set (Item Id)) (Set.Set ItemId) -> IO [ItemId]
    propagate _ (Right s) = return $ Set.toList s
    propagate conn (Left is) = do
      rsts <- mapM (setItem `flip` conn) $ Set.toList is
      let (ers, is') = Either.partitionEithers rsts
      unless (null ers) . fail $ unlines ers
      return $ map itemId is'

    insertArguments :: IConnection conn => conn -> [ItemId] -> SqlValue -> IO ()
    insertArguments conn ids did = do
      let q = "INSERT INTO discussion_arguments (discussionid, itemid) VALUES (?, ?)"
      stmt <- prepare conn q
      executeMany stmt $ map (\i -> [did, toSql i]) ids
