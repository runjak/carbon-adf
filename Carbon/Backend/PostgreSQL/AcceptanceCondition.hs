{-# LANGUAGE RankNTypes #-}
module Carbon.Backend.PostgreSQL.AcceptanceCondition where

import Data.Monoid (Monoid(..))
import qualified Data.Maybe as Maybe

import Carbon.Backend.PostgreSQL.Common
import Carbon.Backend.PostgreSQL.Conversion

getCondition :: SqlValue -> Query (Maybe (AcceptanceCondition Id))
getCondition SqlNull _ = return Nothing
getCondition acid conn = do
  let getAC = "SELECT proofstandard, formula FROM acceptanceconditions WHERE acceptanceconditionid = ?"
  [[ps, f]] <- quickQuery' conn getAC [acid]
  return . Just $ AcceptanceCondition {
    acceptanceConditionId = fromSql acid
  , proofStandard         = fromSql ps
  , formula               = fromSql f
  }

setCondition :: IConnection conn => conn -> Item Id -> IO (Either Error (Item Id))
setCondition conn i
  | Maybe.isNothing (condition i) = do -- Setting condition to null.
    let q = "UPDATE items SET acceptanceconditionid = NULL WHERE itemid = ?"
    quickQuery' conn q [toSql $ itemId i]
    return $ Right i
  | noId . acceptanceConditionId $ getC i = do -- Creating a condition.
    let c = getC i
        q = "INSERT INTO acceptanceconditions (proofstandard, formula) VALUES (?, ?) RETURNING acceptanceconditionid"
    [[cid]] <- quickQuery' conn q [toSql (proofStandard c), toSql $ formula c]
    let q = "UPDATE items SET acceptanceconditionid = ? WHERE itemid = ?"
        cid' = fromSql cid :: Id
    quickQuery' conn q [cid, toSql $ itemId i]
    return . Right $ i <+ c <+ cid'
  | otherwise = do -- Updating the condition.
    let c = getC i
        q = "SELECT COUNT(*) > 0 FROM acceptanceconditions WHERE "
          ++"acceptanceconditionid = ? AND proofstandard = ? AND formula = ?"
    [[t]] <- quickQuery' conn q [toSql $ acceptanceConditionId c, toSql $ proofStandard c, toSql $ formula c]
    let boring  = return $ Right i
        changed = fixParent conn i >> setCondition conn (i <+ c <+ (mempty :: Id))
    fromSql t ? (boring, changed)
  where
    getC = Maybe.fromJust . condition

    {-
      In case we update an acceptance condition, we replace it's parent in discussions, should a parent exist.
    -}
    fixParent :: IConnection conn => conn -> Item Id -> IO ()
    fixParent conn i = do
      let iid  = toSql $ itemId i
          getP = "SELECT MAX(parent) FROM item_family WHERE child = ?"
      pData <- quickQuery' conn getP [iid]
      let hasParents = not $ null pData
      when hasParents $ do
        let parent = toSql . head $ head pData
            update = "UPDATE discussion_arguments SET itemid = ? WHERE itemid = ?"
        void $ quickQuery' conn update [iid, parent]

setCondition' :: Item Id -> Query () 
setCondition' i conn = void $ setCondition conn i
