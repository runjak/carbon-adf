{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Result(
  pageResults, readResult, vote
)where

import OpenBrain.Website.Common
import qualified OpenBrain.Website.Session as Session

pageResults :: OBW Response
pageResults = countAndPageBy ResultCount $ \l o -> liftM responseJSON' $ PageResults l o

readResult :: ResultId -> OBW Response
readResult = respOk . responseJSON' <=< liftB . GetResult

vote :: ResultId -> OBW Response
vote rid = plusm badReq $ do
  uid <- Session.chkSession
  cid <- getChoice
  r   <- liftB $ GetResult rid
  let alreadyVoted = head . (++) [True] . map snd . filter ((==) uid . fst) $ voters r
  plusm cantVote $ do
    guard $ not alreadyVoted
    liftB $ Vote rid uid cid
    readResult rid
  where
    badReq = respBadRequest $ responseJSON "A choice parameter is expected, and the user must be logged in."
    cantVote = respForbidden $ responseJSON "The user either has already voted or was no participant in the discussion."

-- Parameters…
getChoice :: OBW CollectionId
getChoice = lookRead "choice"
