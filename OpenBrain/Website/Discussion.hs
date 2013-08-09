{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Discussion where

import qualified System.Process as Process

import OpenBrain.Website.Common
import qualified OpenBrain.Backend.Logic       as BLogic
import qualified OpenBrain.Data.Logic          as Logic
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
  dInput  <- liftB $ BLogic.diamondInput False did
  dInput' <- liftB $ BLogic.diamondInput True  did
  respOk $ responseJSON' [dInput, dInput']

evaluate :: DiscussionId -> OBW Response
evaluate did = Session.chkSession' . const $ do
  c <- gets config
  let dir  = diamondDlDir c
      call = diamondCall  c
      file = dir ++ "Discussion_" ++ (show . unwrap $ toId did) ++ ".dl"
  fork $ do
    dInput  <- liftB  $ BLogic.diamondInput False did
    dOutput <- liftIO $ do
      writeFile file dInput
      Process.readProcess call [file] ""
    either onError (onResults did) $ Logic.execParser Logic.parseDiamond file dOutput
  respOk $ responseJSON "Evaluating discussion…"
  where
    onError :: String -> Task ()
    onError e = liftIO $
      mapM_ putStrLn ["Could not parse diamond output for discussion "++show did++":",e]

    onResults :: DiscussionId -> Logic.Results String -> Task ()
    onResults did rs = do
      liftIO $ mapM_ putStrLn ["Got diamond results:", show rs]
      liftB  $ BLogic.saveResults did rs

fitInstance :: DiscussionId -> OBW Response
fitInstance did = Session.chkSession' $ \uid -> do
  liftIO $ putStrLn "OpenBrain.Website.Discussion:fitInstance"
  plusm noFile $ do
    content <- getFile
    let source    = "Client upload by user: " ++ show uid
        eInstance = Logic.execParser Logic.parseInstance source content
    either withError (withInstance uid did) eInstance
  where
    noFile = respBadRequest "Expected .dl file is missing."

    withInstance :: UserId -> DiscussionId -> Logic.Instance String -> OBW Response
    withInstance uid did i = liftB (BLogic.fitInstance uid did i) >> respOk "OK"

    withError :: String -> OBW Response
    withError = respBadRequest . toResponse

-- | Parameters…
getDeadline :: OBW (Maybe Timestamp)
getDeadline = msum [liftM Just $ lookRead "deadline", return Nothing]

getFile :: OBW String
getFile = do
  (tmpName, _, _) <- lookFile "file"
  liftIO $ readFile tmpName
