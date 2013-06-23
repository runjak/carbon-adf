{-# LANGUAGE RankNTypes #-}
module OpenBrain.Backend.PostgreSQL.CollectionArticle where

import OpenBrain.Backend.PostgreSQL.Common
import OpenBrain.Data.Id
import OpenBrain.Data.Logic

updatePosition :: CollectionId -> ArticleId -> (Int,Int) -> Query ()
updatePosition cid aid (x,y) conn =
  let q = "UPDATE collectedarticles SET pos_x = ?, pos_y = ? WHERE collectionid = ? AND articleid = ?"
  in void $ quickQuery' conn q [toSql x, toSql y, toSql $ toId cid, toSql $ toId aid]

updateAccepted :: CollectionId -> ArticleId -> Maybe Bool -> Query ()
updateAccepted cid aid mac conn =
  let q = "UPDATE collectedarticles SET accepted = ? WHERE collectionid = ? AND articleid = ?"
  in void $ quickQuery' conn q [toSql mac, toSql $ toId cid, toSql $ toId aid]

updateCondition :: CollectionId -> ArticleId -> Bool -> Exp -> Query ()
updateCondition cid aid custom cond conn =
  let q = "UPDATE collectedarticles SET condition = ?, customcondition = ? WHERE collectionid = ? AND articleid = ?"
  in void $ quickQuery' conn q [toSql $ show cond, toSql custom, toSql $ toId cid, toSql $ toId aid]
