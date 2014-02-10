{-# LANGUAGE OverloadedStrings #-}
module Carbon.Website.Discussion where

import qualified Data.Aeson     as Aeson
import qualified Data.Set       as Set
import qualified Data.String    as String
import qualified System.Process as Process

import Carbon.Website.Common
import qualified Carbon.Backend.Logic       as BLogic
import qualified Carbon.Data.Logic          as Logic
import qualified Carbon.Website.Description as Description
import qualified Carbon.Website.Session     as Session

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
readDiscussion did = do
  d <- liftB $ BLogic.articleIdsToHeadlines =<< GetDiscussion did
  respOk $ responseJSON' d

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

vote :: DiscussionId -> OBW Response
vote did = plusm badReq $ do
  liftIO $ putStrLn "OpenBrain.Website.Discussion:vote"
  uid <- Session.chkSession
  chs <- liftM Set.fromList getChoices
  d   <- liftB $ GetDiscussion did
  -- Check if voting is allowed
  let alreadyVoted = head . (++ [True]) . map snd . filter ((==) uid . fst) $ participants d
  plusm cantVote $ do
    guard $ not alreadyVoted
    -- Filter valid ResultIds from chs
    let valids = Set.fromList . map resultId $ results d
        votes  = Set.toList $ Set.intersection chs valids
    -- Perform vote
    liftB $ mapM_ (Vote `flip` uid) votes
    -- Answer with discussion
    readDiscussion did
  where
    badReq = respBadRequest $ responseJSON'' "A user must be loggedin to vote."
    cantVote = respForbidden $ responseJSON'' "The user either already voted or is no participant."

-- | Parameters…
getDeadline :: OBW (Maybe Timestamp)
getDeadline = msum [liftM Just $ lookRead "deadline", return Nothing]

getFile :: OBW String
getFile = do
  (tmpName, _, _) <- lookFile "file"
  liftIO $ readFile tmpName

getChoices :: OBW [ResultId]
getChoices = do
  cs <- lookRead "choices"
  let mRids = Aeson.decode $ String.fromString cs
  maybe mzero return mRids
