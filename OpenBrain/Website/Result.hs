{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Result(
  pageResults, readResult, vote
)where

import OpenBrain.Website.Common
import qualified OpenBrain.Website.Session as Session

pageResults :: OBW Response
pageResults = countAndPageBy ResultCount $ \l o -> liftM responseJSON' $ PageResults l o

readResult :: ResultId -> OBW Response
readResult rid = do
  rs <- liftB $ GetResults =<< DisForResult rid
  respOk . responseJSON' $ filter ((==) rid . resultId) rs

vote :: ResultId -> OBW Response
vote rid = plusm badReq $ do
  uid <- Session.chkSession
  cid <- getChoice
  d   <- liftB $ GetDiscussion =<< DisForResult rid
  let alreadyVoted = head . (++) [True] . map snd . filter ((==) uid . fst) $ participants d
  plusm cantVote $ do
    guard $ not alreadyVoted
    liftB $ Vote rid uid
    readResult rid
  where
    badReq = respBadRequest  $ responseJSON'' "A choice parameter is expected, and the user must be logged in."
    cantVote = respForbidden $ responseJSON'' "The user either has already voted or was no participant in the discussion."

-- Parameters…
getChoice :: OBW CollectionId
getChoice = lookRead "choice"
