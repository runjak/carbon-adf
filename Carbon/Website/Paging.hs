module Carbon.Website.Paging where

import Carbon.Website.Common

pageItems :: OBW Response
pageItems = do
  p <- getPaging
  plusm (count p) $ do
    l <- lookRead "limit"
    o <- plusm (return 0) $ lookRead "offset"
    is <- liftB $ PageItems p{limit = l, offset = o}
    respOk $ responseJSON' is
  where
    count = respOk . responseJSON' <=< liftB . ItemCount

getPaging :: OBW Paging
getPaging = do
  let bLook = plusm (return False) . lookRead
  article <- bLook "isArticle"
  deleted <- bLook "isDeleted"
  discussion <- bLook "isDiscussion"
  relation <- bLook "isRelation"
  result <- bLook "isResult"
  return Paging {
    isArticle = article
  , isDeleted = deleted
  , isDiscussion = discussion
  , isRelation = relation
  , isResult = result
  , limit = 0
  , offset = 0
  }
