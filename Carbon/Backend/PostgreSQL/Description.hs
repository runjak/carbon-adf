{-# LANGUAGE RankNTypes #-}
module Carbon.Backend.PostgreSQL.Description where

import Data.Monoid (Monoid(..))
import qualified Data.Maybe as Maybe

import Carbon.Backend.PostgreSQL.Common
import Carbon.Backend.PostgreSQL.Conversion

getDescription :: SqlValue -> Query (Maybe Description)
getDescription SqlNull _ = return Nothing
getDescription did conn = do
  let getD = "SELECT headline, summary FROM descriptions WHERE descriptionid = ?"
  [[h,s]] <- quickQuery' conn getD [did]
  return . Just $ Description {
    descriptionId = fromSql did
  , headline      = fromSql h
  , summary       = fromSql s
  }

setDescription :: IConnection conn => conn -> Item Id -> IO (Either Error (Item Id))
setDescription conn i
  | Maybe.isNothing (description i) = do -- Setting description to null.
    let q = "UPDATE items SET descriptionid = NULL WHERE itemid = ?"
    quickQuery' conn q [toSql $ itemId i]
    return $ Right i
  | noId . descriptionId $ getD i = do -- Creating the description.
    let d = getD i
        q = "INSERT INTO descriptions (headline, summary) VALUES (?, ?) RETURNING descriptionid"
    [[did]] <- quickQuery' conn q [toSql (headline d), toSql (summary d)]
    let q = "UPDATE items SET descriptionid = ? WHERE itemid = ?"
        did' = fromSql did :: Id
    quickQuery' conn q [did, toSql $ itemId i]
    return . Right $ i <+ d <+ did'
  | otherwise = do -- Updating the description, if it changed.
    let d = getD i
        q = "SELECT COUNT(*) > 0 FROM descriptions WHERE descriptionid = ? AND headline = ? AND summary = ?"
    [[c]] <- quickQuery' conn q [toSql $ descriptionId d, toSql $ headline d, toSql $ summary d]
    let boring = return $ Right i
        changed = setDescription conn $ i <+ d <+ (mempty :: Id)
    fromSql c ? (boring, changed)
    where
      getD = Maybe.fromJust . description
