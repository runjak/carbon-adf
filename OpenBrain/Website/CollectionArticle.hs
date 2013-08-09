{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.CollectionArticle where

import OpenBrain.Data.Logic
import OpenBrain.Website.Common
import qualified OpenBrain.Backend.Logic   as BLogic
import qualified OpenBrain.Website.Session as Session

updatePosition :: CollectionId -> ArticleId -> OBW Response
updatePosition cid aid = Session.chkSession' . const $
  plusm (respBadRequest "Parameters are: posX, posY") $ do
    liftB . UpdatePosition cid aid =<< getPosition
    respOk "Position updated."

updateCondition :: CollectionId -> ArticleId -> OBW Response
updateCondition cid aid = Session.chkSession' $ \uid ->
  withCondition $ \mCondition -> do
    liftIO $ putStrLn "OpenBrain.Website.CollectionArticle:updateCondition"
    let cToUpdate = maybe (autoConditions aid cid) $ customCondition uid cid aid
    liftB $ cToUpdate mCondition
    respOk "Condition updated."
  where
    autoConditions :: ArticleId -> CollectionId -> BackendDSL ()
    autoConditions aid cid = do
      UpdateCondition cid aid False $ Const False
      mapM_ (`BLogic.autoCondition` aid) =<< DiscussionIds cid

    customCondition :: UserId -> CollectionId -> ArticleId -> Exp String -> BackendDSL ()
    customCondition uid cid aid e = do
      a <- GetArticle aid
      let expToCondition = AC . headline $ aDescription a  :: Exp String   -> ACondition String
          acToInstance   = flip Instance [] . return       :: ACondition a -> Instance a
          i              = acToInstance $ expToCondition e :: Instance String
      mapM_ (BLogic.fitInstance uid `flip` i) =<< DiscussionIds cid

-- | Parameters…
getPosition :: OBW (Int, Int)
getPosition = liftM2 (,) (lookRead "posX") $ lookRead "posY"

withCondition :: (Maybe (Exp String) -> OBW Response) -> OBW Response
withCondition f = plusm (respBadRequest "Parameter condition not found.") $ do
  c <- look "condition"
  case c of
    "" -> f Nothing
    _  ->
      let eCondition = execParser' parseExp "Client Input" c
          problem    = respBadRequest . responseJSON'' . (++) "Could not parse input: "
      in either problem (f . Just) eCondition
