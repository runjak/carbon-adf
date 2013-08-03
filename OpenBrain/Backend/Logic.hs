module OpenBrain.Backend.Logic where

import Control.Arrow ((&&&), second)
import Control.Monad
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe

import OpenBrain.Backend.DSL
import OpenBrain.Common
import OpenBrain.Data
import OpenBrain.Data.Id
import OpenBrain.Data.Logic as Logic

{-|
  Generates the  for a CollectionArticle automatically
  as long as it's not a custom one.
  This will be executed whenever a Relation is added or removed.
|-}
autoCondition :: DiscussionId -> ArticleId -> BackendDSL ()
autoCondition did aid = do
  d <- GetDiscussion did
  let ca = head . filter ((aid ==) . articleId . cArticle) . articles $ dCollection d
  unless (customcondition ca) $ do
    let attackers = map source . filter ((aid ==) . target) $ relations d
        condition = null attackers ? (Const True, Neg . or' $ map idToExp attackers)
    UpdateCondition (collectionId $ dCollection d) aid False condition

{-|
  Generates the content of a diamond input file from a DiscussionId.
|-}
type RenameIds = Bool
diamondInput :: RenameIds -> DiscussionId -> BackendDSL String
diamondInput rIds did = do
  as <- liftM (articles . dCollection) $ GetDiscussion did
  let caToIdName = show . unwrap . toId . articleId . cArticle
      caToHeName = headline . aDescription . cArticle
      renameMap  = Map.fromList $ map (caToIdName &&& caToHeName) as
      hAndMConds = map (caToIdName &&& condition) as
      acs        = map (uncurry AC) hAndMConds
      acs'       = rename renameMap acs
  return . show . instanceFromAcs $ rIds ? (acs', acs)

{-|
  Saves the parsed output as Logic.Results String for a DiscussionId.
  As a first step it also removes older results if existent
|-}
saveResults :: DiscussionId -> Logic.Results String -> BackendDSL ()
saveResults did rs  =
  let (Results rs') = fmap (fromId . wrap . read) rs
  in RemoveResults did >> mapM_ (go did) rs'
  where
    go :: DiscussionId -> (ResultType, [DiamondResult ArticleId]) -> BackendDSL ()
    go did (rType, dResults) = mapM_ (AddResult did rType . mkRArticles) dResults

    mkRArticles :: DiamondResult ArticleId -> [(ResultState, ArticleId)]
    mkRArticles dResult =
      let ins   = map ((,) In)   $ inSet   dResult
          udecs = map ((,) Udec) $ udecSet dResult
          outs  = map ((,) Out)  $ outSet  dResult
      in concat [ins, udecs, outs]
