{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.CollectionArticle where

import OpenBrain.Data.Logic
import OpenBrain.Website.Common
import qualified OpenBrain.Backend.Logic   as Logic
import qualified OpenBrain.Website.Session as Session

updatePosition :: CollectionId -> ArticleId -> OBW Response
updatePosition cid aid = Session.chkSession' . const $
  plusm (respBadRequest "Parameters are: posX, posY") $ do
    liftB . UpdatePosition cid aid =<< getPosition
    respOk "Position updated."

updateCondition :: CollectionId -> ArticleId -> OBW Response
updateCondition cid aid = Session.chkSession' . const $
  withCondition $ \mCondition -> do
    let autoConditions = mapM_ (`Logic.autoCondition` aid) <=< DiscussionIds
        cToUpdate      = maybe (autoConditions cid) $ UpdateCondition cid aid True
    liftB $ cToUpdate mCondition
    respOk "Condition updated."

-- | Parameters…
getPosition :: OBW (Int, Int)
getPosition = liftM2 (,) (lookRead "posX") $ lookRead "posY"

withCondition :: (Maybe Exp -> OBW Response) -> OBW Response
withCondition f = plusm (respBadRequest "Parameter condition not found.") $ do
  c <- look "condition"
  case c of
    "" -> f Nothing
    _  ->
      let eCondition = execParser' parseExp "Client Input" c
          problem    = respBadRequest . responseJSON'' . (++) "Could not parse input: "
      in either problem (f . Just) eCondition
