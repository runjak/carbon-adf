{-# LANGUAGE OverloadedStrings #-}
module Carbon.Website.Item where

import Data.Function (on)
import Data.Monoid (Monoid(..))
import qualified Data.Aeson as Aeson
import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Set   as Set

import Carbon.Backend.Item (hToId, idToH)
import Carbon.Website.Common
import qualified Carbon.Backend.Logic         as BLogic
import qualified Carbon.Backend.Item          as BItem
import qualified Carbon.Data.Logic            as Logic
import qualified Carbon.Data.Logic.Evaluation as Evaluation
import qualified Carbon.Data.ResultSet        as ResultSet
import qualified Carbon.Website.Session       as Session

createItem :: OBW Response
createItem = withItem $ \i -> msum [sane i, setItem i]

-- | FIXME We need to chk if item type is preserved during update
updateItem :: ItemId -> OBW Response
updateItem iid = withItem $ \i -> do
  eEO <- liftB $ GetItem iid
  case eEO of
    (Left e) -> respBadRequest . toResponse $ unlines ["Cannot update requested item, because it cannot be found.", e]
    (Right o) -> do
      o' <- liftB $ idToH o
      let i' = o' `mappend` (i <+ iid)
      msum [sane i', okChange o' i', setItem i']
  where
    okChange :: Item String -> Item String -> OBW Response
    okChange old new = do
      eEU <- liftB . GetUser $ commitAuthor new
      case eEU of
        (Left e) -> respBadRequest . toResponse $ unlines ["Cannot find commitAuthor on the server.",e]
        (Right u) -> do
          let okDisc = (okDDelta u `on` discussion) old new
              okResS = (ResultSet.okRDelta u `on` resultSet) old new
          when (okDisc && okResS) mzero
          let repDisc = okDisc ? (["Changes to the Discussion were accepted."],["Changes on the Discussion were not accepted."])
              repResS = okResS ? (["Changes to the ResultSet were accepted."],["Changes on the ResultSet were not accepted."])
          liftIO $ (ResultSet.rDeltaTable u `on` resultSet) old new
          respBadRequest . toResponse . unlines $ repDisc++repResS

    okDDelta :: User -> Maybe (Discussion (Item String)) -> Maybe (Discussion (Item String)) -> Bool
    okDDelta _ Nothing _        = True
    okDDelta _ (Just _) Nothing = False
    {- If the user is an admin, we request that discussionId and evaluation stay the same.
      Otherwise deadline must be the same and participants may only differ by the current user. -}
    okDDelta u (Just old) (Just new)
      | isAdmin u = sameId old new && sameEvaluation old new
      | otherwise = and [sameId old new, sameEvaluation old new, sameDeadline old new, saneParticipants u old new]
      where
        sameId             = (==) `on` discussionId
        sameEvaluation     = (==) `on` evaluation
        sameDeadline       = (==) `on` deadline
        saneParticipants u = (==) `on` (Set.delete (userId u) . participants)

readItem :: ItemId -> OBW Response
readItem iid = do
  eEI <- liftB $ GetItem iid
  case eEI of
    (Right i) -> do
      i' <- liftB $ BItem.mapArgs i BLogic.autoCondition
      displayItem i'
    (Left  e) -> respInternalServerError $ toResponse e

deleteItem :: ItemId -> OBW Response
deleteItem iid = do
  mE <- liftB $ DeleteItem iid
  case mE of
    Nothing -> respOk . toResponse $ concat ["Item ", show iid, " deleted."]
    (Just e) -> respInternalServerError $ toResponse e

-- Handling items:
withItem :: (Item String -> OBW Response) -> OBW Response
withItem onItem = Session.chkSession' $ \uid -> do
  eEItem <- getItem uid
  let onError = respBadRequest . toResponse
  either onError onItem eEItem

setItem :: Item String -> OBW Response
setItem i = do
  liftIO $ putStrLn "Carbon.Website.Item:setItem"
  liftIO $ print i
  eEI <- liftB $ SetItem =<< BLogic.autoCondition =<< hToId i
  let no = respInternalServerError . toResponse
  either no displayItem eEI

displayItem :: Item Id -> OBW Response
displayItem i = do
  i' <- liftB $ idToH i
  respOk $ responseJSON' i'

-- sane :: Item a -> OBW Response
sane :: Item String -> OBW Response
sane i
  | itemIsSane i = mzero
  | otherwise = do
      liftIO $ putStrLn "Insane Item found:" >> sanityTable i
      respBadRequest "Sorry, but the given Item is not considered sane by the server."

-- Reading get parameters:
getItem :: UserId -> OBW (Either Error (Item String))
getItem author = do
  -- Reading parts:
  mDescription <- getFromJSON "description"
  mArticle     <- getFromJSON "article"
  mCondition   <- getFromJSON "condition"
  mRelation    <- getFromJSON "relation"
  mDiscussion  <- getFromJSON "discussion"
  mResultSet   <- getFromJSON "resultSet"
  plusm (return $ Left "Got no commit Message") $ do
    cMsg <- look "commitMessage"
    -- Constructing Item:
    let i = Item{
        itemId        = mempty
      , description   = mDescription
      , article       = mArticle
      , condition     = mCondition
      , relation      = mRelation
      , relations     = []
      , discussion    = mDiscussion
      , resultSet     = mResultSet
      , creation      = mempty
      , deletion      = mempty
      , parents       = mempty
      , children      = mempty
      , commitMessage = cMsg
      , commitAuthor  = author
      }
    return $ Right i
  where
    getFromJSON :: (Aeson.FromJSON a) => String -> OBW (Maybe a)
    getFromJSON = plusm (return Nothing) . liftM Aeson.decode . lookBS

fitInstance :: ItemId -> OBW Response
fitInstance iid = Session.chkSession' $ \uid -> do
  liftIO $ putStrLn "Carbon.Website.Item:fitInstance"
  eEI <- liftB $ GetItem iid
  case eEI of
    (Left e) -> respBadRequest $ toResponse e
    (Right i) -> plusm noDisc $ do
      guard $ itemIsDiscussion i
      plusm noFile $ do
        f <- getFile
        let source    = "Client upload by user: " ++ show uid
            eInstance = Logic.execParser Logic.parseInstance source f
        either withError (withInstance uid i) eInstance
  where
    noDisc = respBadRequest "Item is no discussion."
    noFile = respBadRequest "Expected .dl file is missing."

    withError :: String -> OBW Response
    withError = respBadRequest . toResponse

    withInstance :: UserId -> Item Id -> Instance Headline -> OBW Response
    withInstance uid item inst = do
      eEI <- liftB $ BLogic.fitInstance uid item inst
      case eEI of
        (Right i) -> respOk $ responseJSON' i
        (Left e) -> respInternalServerError $ toResponse e

getFile :: OBW String
getFile = do
  (tmpName, _, _) <- lookFile "file"
  liftIO $ readFile tmpName

acs :: ItemId -> OBW Response
acs = respOk . responseJSON' <=< liftB . BLogic.diamondInput

{-|
  Should be called upon a discussion,
  which will then be evaluated.
|-}
evaluate :: ItemId -> OBW Response
evaluate iid = do
  liftIO $ putStrLn "Carbon.Website.Item:evaluate"
  (input, _) <- liftB $ BLogic.diamondInput iid
  config <- gets config
  let path = diamondDlDir config ++ show iid ++ ".dl"
  results <- liftIO $ Evaluation.run config path input
  let rSet = fromResults $ fmap read results
  eEI <- liftB $ do
    (Right i) <- GetItem iid
    SetItem . addVoters $ i <+ rSet
  case eEI of
    (Right i) -> displayItem i
    (Left  e) -> respInternalServerError $ toResponse e
  where
    -- only works when: itemIsDiscussion i, itemIsResult i
    addVoters :: Item Id -> Item Id
    addVoters i = let ps  = participants . Maybe.fromJust $ discussion i
                      ps' = zip (Set.toList ps) $ repeat False
                      rs  = Maybe.fromJust $ resultSet i
                  in i <+ rs{voters = ps'}
