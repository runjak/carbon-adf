{-# LANGUAGE RankNTypes #-}
module Carbon.Backend.PostgreSQL.Relation where

import Data.Monoid (Monoid(..))
import qualified Data.Maybe as Maybe

import Carbon.Backend.PostgreSQL.Common
import Carbon.Backend.PostgreSQL.Conversion

getRelation :: SqlValue -> Query (Maybe Relation)
getRelation SqlNull _ = return Nothing
getRelation rid conn = do
  let getR = "SELECT source, target, relationtype FROM relations WHERE relationid = ?"
  [[s, t, rt]] <- quickQuery' conn getR [rid]
  return . Just $ Relation {
    relationId   = fromSql rid
  , source       = fromSql s
  , target       = fromSql t
  , relationType = fromSql rt
  }

-- When creating/updating a relation,
-- we must check that it doesn't link to any item that has itself a relation,
-- otherwise cloning will run into a loop.
setRelation :: IConnection conn => conn -> Item Id -> IO (Either Error (Item Id))
setRelation conn i
  | Maybe.isNothing (relation i) = do -- Setting relation to null.
    let q = "UPDATE items SET relationid = NULL WHERE itemid = ?"
    quickQuery' conn q [toSql $ itemId i]
    return $ Right i
  | noId . relationId $ getR i = do -- Creating a relation.
    let r = getR i
    performChk conn r $ do
      let q = "INSERT INTO relations (source, target, relationtype) VALUES (?, ?, ?) RETURNING relationid"
      [[rid]] <- quickQuery' conn q [toSql $ source r, toSql $ target r, toSql $ relationType r]
      let q = "UPDATE items SET relationid = ? WHERE itemid = ?"
          rid' = fromSql rid :: Id
      quickQuery' conn q [rid, toSql $ itemId i]
      return . Right $ i <+ r <+ rid'
  | otherwise = do -- Updating a relation.
    let r = getR i
        q = "SELECT COUNT(*) > 0 FROM relations WHERE relationid = ? AND source = ? AND target = ? AND relationtype = ?"
    [[c]] <- quickQuery' conn q [toSql $ relationId r, toSql $ source r, toSql $ target r, toSql $ relationType r]
    let boring = return $ Right i
        changed = setRelation conn $ i <+ r <+ (mempty :: Id)
    fromSql c ? (boring, changed)
  where
    getR = Maybe.fromJust . relation
    performChk conn r cont = do
      let q = "SELECT COUNT(*) = 2 FROM items WHERE relationid IS NULL AND (itemid = ? OR itemid = ?)"
          problem = return $ Left "Relation cannot link to/from an Item that is itself a Relation!"
      [[count]] <- quickQuery' conn q [toSql $ source r, toSql $ target r]
      fromSql count ? (cont, problem)
