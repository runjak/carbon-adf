{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.CollectionArticle where

import OpenBrain.Data.Logic
import OpenBrain.Website.Common
import qualified OpenBrain.Website.Session as Session

updatePosition :: CollectionId -> ArticleId -> OBW Response
updatePosition cid aid = Session.chkSession' . const $
  plusm (respBadRequest "Parameters are: pos_x, pos_y") $ do
    liftB . UpdatePosition cid aid =<< getPosition
    respOk "Position updated."

updateCondition :: CollectionId -> ArticleId -> OBW Response
updateCondition cid aid = Session.chkSession' . const $
  plusm (respBadRequest "Parameter condition not found.") $ do
    let cToUpdate = maybe (UpdateCondition cid aid False $ Var "") (UpdateCondition cid aid True)
    liftB =<< liftM cToUpdate getCondition
    respOk "Condition updated."

-- | Parameters…
getPosition :: OBW (Int, Int)
getPosition = liftM2 (,) (lookRead "pos_x") $ lookRead "pos_y"

getCondition :: OBW (Maybe Exp)
getCondition = plusm (return Nothing) . liftM Just $ lookRead "condition"
