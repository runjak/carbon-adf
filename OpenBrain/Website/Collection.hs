{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Collection where

import OpenBrain.Website.Common
import qualified OpenBrain.Backend.Logic   as BLogic
import qualified OpenBrain.Website.Session as Session

pageCollections :: OBW Response
pageCollections = countAndPageBy CollectionCount $ \l o -> liftM responseJSON' $ PageCollections l o

readCollection :: CollectionId -> OBW Response
readCollection cid = do
  c <- liftB $ BLogic.articleIdsToHeadlines =<< GetCollection cid 
  respOk $ responseJSON' c

addToCollection :: CollectionId -> ArticleId -> OBW Response
addToCollection cid aid = Session.chkSession' . const $ do
  liftB $ CollectArticles cid [aid]
  readCollection cid

delFromCollection :: CollectionId -> ArticleId -> OBW Response
delFromCollection cid aid = Session.chkSession' . const $ do
  liftB $ ForgetArticles cid [aid]
  readCollection cid
