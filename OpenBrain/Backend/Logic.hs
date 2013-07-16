module OpenBrain.Backend.Logic where

import Control.Monad

import OpenBrain.Backend.DSL
import OpenBrain.Data
import OpenBrain.Data.Id
import OpenBrain.Data.Logic

{-|
  Generates the condition for a CollectionArticle automatically
  as long as it's not a custom one.
  This will be executed whenever a Relation is added or removed.
|-}
autoCondition :: DiscussionId -> ArticleId -> BackendDSL ()
autoCondition did aid = do
  d <- GetDiscussion did
  let ca = head . filter ((aid ==) . articleId . cArticle) . articles $ dCollection d
  when (not $ customcondition ca) $ do
    let attackers = map (source) . filter ((aid ==) . target) $ relations d
    let condition = Not . or' $ map idToExp attackers
    UpdateCondition (collectionId $ dCollection d) aid False condition
