{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Discussion where

import qualified System.Process as Process

import OpenBrain.Website.Common
import qualified OpenBrain.Backend.Logic       as Logic
import qualified OpenBrain.Website.Description as Description
import qualified OpenBrain.Website.Session     as Session

pageDiscussions :: OBW Response
pageDiscussions = countAndPageBy DiscussionCount $ \l o -> liftM responseJSON' $ PageDiscussions l o

createDiscussion :: OBW Response
createDiscussion = Session.chkSession' $ \uid -> plusm createFail $ do
  liftIO $ putStrLn "OpenBrain.Website.Discussion:createDiscussion"
  (deadline, ndid) <- liftM2 (,) getDeadline Description.createDescription
  did <- liftB $ do
    ncid <- AddCollection ndid []
    AddDiscussion ncid [uid] deadline
  readDiscussion did
  where
    createFail = respBadRequest $ responseJSON'' "Expected parameters: headline, description, [deadline]"

readDiscussion :: DiscussionId -> OBW Response
readDiscussion = respOk . responseJSON' <=< liftB . GetDiscussion

joinDiscussion :: DiscussionId -> OBW Response
joinDiscussion did = Session.chkSession' $ \uid -> do
  liftIO $ putStrLn "OpenBrain.Website.Discussion:joinDiscussion"
  liftB $ SetParticipant did uid True
  readDiscussion did

leaveDiscussion :: DiscussionId -> OBW Response
leaveDiscussion did = Session.chkSession' $ \uid -> do
  liftIO $ putStrLn "OpenBrain.Website.Discussion:leaveDiscussion"
  liftB $ SetParticipant did uid False
  readDiscussion did

acs :: DiscussionId -> OBW Response
acs did = do
  dInput  <- liftB $ Logic.diamondInput False did
  dInput' <- liftB $ Logic.diamondInput True  did
  respOk $ responseJSON' [dInput, dInput']

evaluate :: DiscussionId -> OBW Response
evaluate did = Session.chkSession' . const $ do
  c <- gets config
  let dir  = diamondDlDir c
      call = diamondCall  c
      file = dir ++ "Discussion_" ++ (show . unwrap $ toId did) ++ ".dl"
  dInput  <- liftB  $ Logic.diamondInput False did
  dOutput <- liftIO $ do
    writeFile file dInput
    Process.readProcess call [file] ""
  respOk . responseJSON' $ "Created file: "++file++"\nDiamond Output was:\n"++dOutput

-- | Parameters…
getDeadline :: OBW (Maybe Timestamp)
getDeadline = msum [liftM Just $ lookRead "deadline", return Nothing]
