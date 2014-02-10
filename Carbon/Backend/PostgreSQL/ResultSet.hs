{-# LANGUAGE RankNTypes #-}
module Carbon.Backend.PostgreSQL.ResultSet where

import Data.Monoid (Monoid(..))
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Carbon.Backend.PostgreSQL.Common
import Carbon.Backend.PostgreSQL.Conversion

getResultSet :: SqlValue -> Query (Maybe ResultSet)
getResultSet SqlNull _ = return Nothing
getResultSet rsid conn = do
  let getCreation = "SELECT setcreation FROM resultsets WHERE resultsetid = ?"
      getResultIds = "SELECT resultid FROM resultset_results WHERE resultsetid = ?"
      getVoters = "SELECT userid, voted FROM resultset_voters WHERE resultsetid = ?"
  [[creation]] <- quickQuery' conn getCreation [rsid]
  rids <- liftM (map head) $ quickQuery' conn getResultIds [rsid]
  vs <- liftM (map (\[u,v] -> (fromSql u, fromSql v))) $ quickQuery' conn getVoters [rsid]
  rs <- forM rids $ getResult `flip` conn
  return . Just $ ResultSet {
    resultSetId = fromSql rsid
  , setCreation = fromSql creation
  , results     = rs
  , voters      = vs
  }
  where
    getResult :: SqlValue -> Query Result
    getResult rid conn = do
      let getR = "SELECT resulttype, votes FROM results WHERE resultid = ?"
          getItems = "SELECT resultstate, itemid FROM results_items WHERE resultid = ?"
      [[rtype, votes]] <- quickQuery' conn getR [rid]
      is' <- quickQuery' conn getItems [rid]
      let is = Set.fromList $ map (\[rs, iid] -> (fromSql rs, fromSql iid)) is'
      return Result {
        resultId   = fromSql rid
      , resultType = fromSql rtype
      , items      = is
      , votes      = fromSql votes
      }

setResultSet :: IConnection conn => conn -> Item Id -> IO (Either Error (Item Id))
setResultSet conn i
  | Maybe.isNothing (resultSet i) = do -- Set resultSet to NULL
    let q = "UPDATE items SET resultsetid = NULL WHERE itemid = ?"
    quickQuery' conn q [toSql $ itemId i]
    return $ Right i
  | noId . resultSetId $ getR i = do -- Create resultSet
    let r = getR i
        q = "INSERT INTO resultsets (setcreation) VALUES (now()) RETURNING resultsetid"
    [[rid]] <- quickQuery' conn q []
    let rid' = fromSql rid :: ResultSetId
    setResultSet conn $ i <+ r <+ rid'
  | otherwise = do -- Update resultSet
    let r = getR i
    r' <- liftM Maybe.fromJust $ getResultSet (toSql $ resultSetId r) conn
    let boring = return $ Right i
        changed = setResultSet conn $ i <+ r <+ (mempty :: Id)
    (r == r') ? (boring, changed)
  where
    getR = Maybe.fromJust . resultSet
    
    setVoters :: IConnection conn => conn -> SqlValue -> [(UserId, Voted)] -> IO ()
    setVoters conn rid vs = do
      let q = "DELETE FROM resultset_voters WHERE resultsetid = ?"
      quickQuery' conn q [rid]
      stmt <- prepare conn "INSERT INTO resultset_voters (resultsetid, userid, voted) VALUES (?, ?, ?)"
      executeMany stmt $ map (\(uid, v) -> [rid, toSql uid, toSql v]) vs

    setResult :: IConnection conn => conn -> Result -> IO Result
    setResult conn r
      | Maybe.isNothing (resultId r) = do -- Create the result
        let q = "INSERT INTO results (resulttype, votes) VALUES (?, ?) RETURNING resultid"
        [[rid]] <- quickQuery' conn q [toSql (resultType r), toSql $ votes r]
        setItems conn rid . Set.toList $ items r
        let rid' = fromSql rid :: Id
        return $ r <+ rid'
      | otherwise = do
        let q = "UPDATE results SET resulttype = ?, votes = ? WHERE resultid = ?"
            rid = toSql . Maybe.fromJust $ resultId r
        quickQuery' conn q [toSql (resultType r), toSql (votes r), rid]
        setItems conn rid . Set.toList $ items r
        return r
      where
        setItems :: IConnection conn => conn -> SqlValue -> [(ResultState, Id)] -> IO ()
        setItems conn r is = do
          let q = "DELETE FROM results_items WHERE resultid = ?"
          quickQuery' conn q [r]
          stmt <- prepare conn "INSERT INTO results_items (resultid, resultstate, itemid) VALUES (?, ?, ?)"
          executeMany stmt $ map (\(rs, iid) -> [r, toSql rs, toSql iid]) is

    setResults :: IConnection conn => conn -> SqlValue -> [Id] -> IO ()
    setResults conn rsid rs = do
      let q = "DELETE FROM resultset_results WHERE resultsetid = ?"
      quickQuery' conn q [rsid]
      stmt <- prepare conn "INSERT INTO resultset_results (resultsetid, resultid) VALUES (?, ?)"
      executeMany stmt $ map (\r -> [rsid, toSql r]) rs
