{-# LANGUAGE RankNTypes #-}
module OpenBrain.Backend.PostgreSQL.Description where

import OpenBrain.Backend.PostgreSQL.Common
import OpenBrain.Data.Id

addDescription :: Author -> Headline -> String -> Query NewDescriptionId
addDescription author headline desc conn = do
  let q = "INSERT INTO descriptions (author, headline, description) VALUES (?, ?, ?) RETURNING descriptionid"
  [[i]] <- quickQuery' conn q [toSql $ toId author, toSql headline, toSql desc]
  return . fromId $ fromSql i

deleteDescription :: DescriptionId -> Query ()
deleteDescription did conn = 
  let q = "UPDATE descriptions SET deletion = now() WHERE descriptionid = ?"
  in void $ quickQuery' conn q [toSql $ toId did]

getDescription :: DescriptionId -> Query Description
getDescription did conn = do
  let q = "SELECT author, headline, description, creation, deletion FROM descriptions WHERE descriptionid = ?"
  [[a, h, d, ctime, dtime]] <- quickQuery' conn q [toSql $ toId did]
  return Description{
    descriptionId = did
  , author        = fromId $ fromSql a
  , headline      = fromSql h
  , description   = fromSql d
  , creationTime  = fromSql ctime
  , deletionTime  = fromSql dtime
  }

setHeadline :: DescriptionId -> Headline -> Query ()
setHeadline did h conn =
  let q = "UPDATE descriptions SET headline = ? WHERE descriptionid = ?"
  in void $ quickQuery' conn q [toSql h, toSql $ toId did]

setDescription :: DescriptionId -> String -> Query ()
setDescription did desc conn =
  let q = "UPDATE descriptions SET description = ? WHERE descriptionid = ?"
  in void $ quickQuery' conn q [toSql desc, toSql $ toId did]
